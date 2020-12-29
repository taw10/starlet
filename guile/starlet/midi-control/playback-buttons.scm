(define-module (starlet midi-control playback-buttons)
  #:use-module (starlet midi-control base)
  #:use-module (starlet playback)
  #:export (make-go-button
            make-stop-button))


(define* (make-go-button pb button
                         #:key (channel #f))
    (register-midi-note-callback!
     #:channel channel
     #:note-number button
     #:func (lambda () (go! pb))))


(define* (make-stop-button pb button
                           #:key (channel #f))
    (register-midi-note-callback!
     #:channel channel
     #:note-number button
     #:func (lambda () (display "Stop/back!\n"))))
