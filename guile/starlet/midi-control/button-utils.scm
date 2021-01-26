(define-module (starlet midi-control button-utils)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:use-module (starlet playback)
  #:export (make-go-button
            make-stop-button
            select-on-button))


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


(define* (select-on-button button fixture
                           #:key (channel #f))
  (register-midi-note-callback!
   #:channel channel
   #:note-number button
   #:func (lambda () (sel fixture))))
