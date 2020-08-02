(define-module (guile-midi control)
  #:use-module (oop goops)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:export (start-midi-control
            make-midi-controller
            get-controller-value
            make-midi-led
            set-midi-led!))


(define cc-list (make-atomic-box '()))
(define send-queue (make-atomic-box '()))


(define-class <midi-led> (<object>)

  (channel
   #:init-keyword #:channel
   #:getter get-channel)

  (note-number
   #:init-keyword #:note-number
   #:getter get-note-number))


(define-class <midi-control> (<object>)

  (channel
   #:init-keyword #:channel
   #:getter get-channel)

  (cc-number
   #:init-keyword #:cc-number
   #:getter get-cc-number)

  (value-box
   #:init-form (make-atomic-box 0)
   #:getter get-value-box))


(define (get-controller-value a)
  (atomic-box-ref (get-value-box a)))


(define* (make-midi-controller
          #:key (channel 1) (cc-number 1))
  (let ((new-controller (make <midi-control>
                          #:channel channel
                          #:cc-number cc-number)))
    (atomic-box-set! cc-list
                     (cons new-controller
                           (atomic-box-ref cc-list)))
    new-controller))


(define* (make-midi-led
          #:key (channel 1) (note-number 1))
  (let ((new-led (make <midi-led>
                   #:channel channel
                   #:note-number note-number)))
    new-led))


(define enqueue-midi-bytes
  (lambda bytes
    (unless (eq? (atomic-box-compare-and-swap! send-queue '() bytes)
                 '())
      (apply enqueue-midi-bytes bytes))))


(define (set-midi-led! led val)
  (if val

      ;; Note on
      (enqueue-midi-bytes (+ #b10010000 (get-channel led))
                          (get-note-number led)
                          127)

      ;; Note off
      (enqueue-midi-bytes (+ #b10000000 (get-channel led))
                          (get-note-number led)
                          0)))


(define (handle-cc-change channel cc-number value)
  (for-each (lambda (a)
              (atomic-box-set! (get-value-box a) value))
            (filter (lambda (a)
                      (and (eq? cc-number (get-cc-number a))
                           (eq? channel (get-channel a))))
                    (atomic-box-ref cc-list))))


(define (scale-127-100 n)
  (/ (* n 100) 127))


(define (start-midi-control device-name)
  (let ((midi-port (open-file device-name "r+0b")))

    ;; Read thread
    (begin-thread
     (let again ()

       (letrec* ((status-byte (get-u8 midi-port))
                 (channel (bit-extract status-byte 0 4))
                 (command (bit-extract status-byte 4 8)))

         (case command

           ;; Note on
           ((9) (let ((note (get-u8 midi-port))
                      (vel (get-u8 midi-port)))
                  (display "Note = ")
                  (display (number->string note 16))
                  (display " velocity = ")
                  (display vel)
                  (newline)))

           ;; Control value
           ((11) (let* ((cc-number (get-u8 midi-port))
                        (value (get-u8 midi-port)))
                   (handle-cc-change channel
                                     cc-number
                                     (scale-127-100 value)))))

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
