(define-module (starlet midi-control base)
  #:use-module (oop goops)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:export (start-midi-control
            get-cc-value
            scale-127-100
            send-note-on
            send-note-off
            register-midi-note-callback!))


(define cc-arrays (make-atomic-box '()))
(define callback-list (make-atomic-box '()))
(define send-queue (make-atomic-box '()))


(define-class <midi-note-callback> (<object>)

  (channel
   #:init-keyword #:channel
   #:getter get-channel)

  (note-number
   #:init-keyword #:note-number
   #:getter get-note-number)

  (callback
   #:init-keyword #:func
   #:getter get-callback-func))


(define* (register-midi-note-callback!
          #:key (channel #f) (note-number 1) (func #f))
  (let ((new-callback (make <midi-note-callback>
                        #:channel (if channel channel default-channel)
                        #:note-number note-number
                        #:func func)))
    (atomic-box-set! callback-list
                     (cons new-callback
                           (atomic-box-ref callback-list)))
    new-callback))


(define enqueue-midi-bytes!
  (lambda bytes
    (unless (eq? (atomic-box-compare-and-swap! send-queue '() bytes)
                 '())
      (apply enqueue-midi-bytes! bytes))))


(define* (send-note-on note
                       #:key (channel #f))
  (enqueue-midi-bytes! (+ #b10010000
                          (if channel channel default-channel))
                       note
                       127))


(define* (send-note-off note
                        #:key (channel #f))
  (enqueue-midi-bytes! (+ #b10000000
                          (if channel channel default-channel))
                       note
                       0))


(define (all-notes-off! channel)
  (let again ((l 0))
    (enqueue-midi-bytes! (+ #b10000000 channel) l 0)
    (unless (= l 127)
      (again (+ l 1)))))


(define (ensure-cc-array channel)
  (let ((old-list (atomic-box-ref cc-arrays)))
    (unless (assq channel old-list)
      (unless (eq?
               old-list
               (atomic-box-compare-and-swap! cc-arrays
                                             old-list
                                             (acons channel
                                                    (make-vector 128 #f)
                                                    old-list)))
        ;; CAS failed - try again
        (ensure-cc-array channel)))))


(define (handle-cc-change! channel cc-number value)
  (ensure-cc-array channel)
  (vector-set! (assq-ref (atomic-box-ref cc-arrays) channel)
               cc-number
               value))


(define* (get-cc-value cc-number
                       #:key (channel #f))
  (let ((cc-arrays (atomic-box-ref cc-arrays)))
    (let ((ccs (assq-ref cc-arrays
                         (if channel channel default-channel))))
      (if ccs
          (vector-ref ccs cc-number)
          0))))


(define (check-note-callbacks channel note-number)
  (for-each (lambda (a) ((get-callback-func a)))
            (filter (lambda (a)
                      (and (eq? note-number (get-note-number a))
                           (eq? channel (get-channel a))))
                    (atomic-box-ref callback-list))))


(define (scale-127-100 n)
  (/ (* n 100) 127))


(define default-channel 0)

(define* (start-midi-control device-name
                             #:key (channel #f))

  (when channel
    (set! default-channel channel))

  (let ((midi-port (open-file device-name "r+0b")))

    ;; Read thread
    (begin-thread
     (let again ()

       (let* ((status-byte (get-u8 midi-port))
              (channel (bit-extract status-byte 0 4))
              (command (bit-extract status-byte 4 8)))

         (case command

           ;; Note on
           ((9) (let* ((note (get-u8 midi-port))
                       (vel (get-u8 midi-port)))
                  (check-note-callbacks channel note)))

           ;; Control value
           ((11) (let* ((cc-number (get-u8 midi-port))
                        (value (get-u8 midi-port)))
                   (handle-cc-change! channel
                                      cc-number
                                      value))))

         (yield)
         (again))))

    ;; Write thread
    (begin-thread
     (let again ()
       (let ((bytes-to-send (atomic-box-swap! send-queue '())))
         (for-each (lambda (a)
                     (put-u8 midi-port a))
                   bytes-to-send)
         (usleep 1000)
         (again))))))
