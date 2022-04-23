(define-module (taw controls)
  #:use-module (starlet midi-control base)
  #:use-module (starlet midi-control button-utils)
  #:use-module (starlet midi-control faders)
  #:export (taw-playback-controls))


(define (taw-playback-controls controller pb)

  (make-go-button controller pb 12
                  #:ready-note 20
                  #:pause-note 16)
  (make-stop-button controller pb 24
                    #:ready-note 24)
  (make-back-button controller pb 28
                    #:ready-note 28)

  (make-go-button controller pb 15
                  #:ready-note 23
                  #:pause-note 19)
  (make-stop-button controller pb 27
                    #:ready-note 27)
  (make-back-button controller pb 31
                    #:ready-note 31))


