(define-module (guile-midi control)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 binary-ports)
  #:export (make-midi-port
            midi-cc-value
            send-midi-note))


(define (make-midi-port device-name listen-channel)

  (let ((cc-vals (make-array 0 128))
        (midi-port (open-file device-name "r+0b")))


    ;; Send a note off command
    (define (send-noteoff note)
      (put-u8 midi-port
              (+ #b10000000 listen-channel))
      (put-u8 midi-port note)
      (put-u8 midi-port 0))


    ;; Send a note on command
    (define (send-note note velocity)
      (put-u8 midi-port
              (+ #b10010000 listen-channel))
      (put-u8 midi-port note)
      (put-u8 midi-port velocity))


    ;; Get a CC value
    (define (get-cc-value controller-number)
      (array-ref cc-vals
                 controller-number))


    (define (run-midi)

        (let again ()
          (letrec* ((status-byte (get-u8 midi-port))
                    (channel (bit-extract status-byte 0 4))
                    (command (bit-extract status-byte 4 8)))

            (when (eq? channel listen-channel)
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
                ((11) (let* ((controller-number (get-u8 midi-port))
                             (value (get-u8 midi-port)))
                        (array-set! cc-vals
                                    value
                                    controller-number)))))


            (again))))

    ;; Clear out any LEDs by first sending note-on with velocity zero
    (for-each (lambda (n)
                (send-note n 0))
              (iota 128 0))

    ;; ... and subsequently sending note-off
    (for-each (lambda (n)
                (send-noteoff n))
              (iota 128 0))

    (make-thread run-midi)

    (lambda args
      (apply
       (case (car args)
         ((get-cc-value) get-cc-value)
         ((send-note) send-note))
       (cdr args)))))


(define-syntax midi-cc-value
  (lambda (x)
    (syntax-case x ()
      ((_ port controller-number)
       #'(port 'get-cc-value controller-number)))))


(define-syntax send-midi-note
  (lambda (x)
    (syntax-case x ()

      ((_ port note velocity)
       #'(port 'send-note note velocity))

      ((_ port note)
       #'(port 'send-note note 127)))))
