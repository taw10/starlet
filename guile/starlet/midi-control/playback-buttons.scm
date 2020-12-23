(define-module (starlet midi-control playback-buttons)
  #:use-module (starlet midi-control base)
  #:use-module (starlet playback)
  #:export (make-midi-playback-buttons))


(define (make-midi-playback-buttons playback
                                    chan
                                    go-led-note
                                    go-button-note
                                    stop-led-note
                                    stop-button-note)
  (let ((go-led (make-midi-led #:channel chan
                               #:note-number go-led-note))
        (stop-led (make-midi-led #:channel chan
                                 #:note-number stop-led-note)))
    (set-midi-led! go-led #t)
    (set-midi-led! stop-led #t)
    (register-midi-note-callback!
     #:channel chan
     #:note-number go-button-note
     #:func (lambda () (go! playback)))
    (register-midi-note-callback!
     #:channel chan
     #:note-number stop-button-note
     #:func (lambda () (display "Stop/back!\n")))))
