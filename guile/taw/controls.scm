(define-module (taw controls)
  #:use-module (starlet attributes)
  #:use-module (starlet playback)
  #:use-module (starlet colours)
  #:use-module (starlet midi-control base)
  #:use-module (starlet midi-control button-utils)
  #:use-module (starlet midi-control faders)
  #:export (taw-playback-controls
             taw-selection-controls))


(define (taw-playback-controls controller pb)

  (register-midi-note-callback!
    controller
    #:note-number 25
    #:func (lambda ()
             (reload-cue-list! pb)
             (reassert-current-cue! pb)))

  (send-note-on controller 25)

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


(define (taw-selection-controls controller)

  ;; Red button de-selects everything
  (select-on-button controller 26 #f
                    #:ready-note 26)

  ;; My control map
  (set-midi-control-map!
    controller
    (fader 16 intensity                      #:congruent 108  #:incongruent 72)
    (jogwheel  0 intensity                   #:active 124)
    (fader 4  (colour-component-id 'cyan)    #:congruent 120  #:incongruent 84)
    (fader 5  (colour-component-id 'magenta) #:congruent 121  #:incongruent 85)
    (fader 6  (colour-component-id 'yellow)  #:congruent 122  #:incongruent 86)
    (fader 7  colour-temperature             #:congruent 123  #:incongruent 87)))
