;; FIXME: Obviously, a better way of loading is needed
(add-to-load-path "/home/taw/starlet/guile")
(add-to-load-path "/home/taw/starlet/shows")

(use-modules
 (starlet base)
 (starlet playback)
 (starlet midi-control base)
 (starlet midi-control playback-buttons)
 (starlet midi-control faders)
 (demo-show patch))

(define cue-list-module
  (resolve-module '(demo-show cue-list) #t))

(module-use! (current-module) cue-list-module)

;; Create playback
(define pb
  (make-playback my-cue-list))
(register-state! pb)
(cut-to-cue-number! pb 0)

;; Start readout to OLA
(start-ola-output)

;; Start MIDI control
(start-midi-control "/dev/snd/midiC1D0"
                    #:channel 14)

;;;; Set up cue list go/stop buttons, and light up LEDs to show
(make-go-button pb 12)
(make-stop-button pb 24)
(send-note-on 20)
(send-note-on 24)

;; A second set of go/stop buttons
(make-go-button pb 15)
(make-stop-button pb 27)
(send-note-on 23)
(send-note-on 27)

;; Set up a fader for quick access to some working light
(on-fader 19
          (lighting-state
           (at dim1 100)
           (at dim2 100)
           (at dim3 100)
           (at dim10 50)
           (at dim11 50)
           (at dim12 50)))

(define (reload-cue-list)
  (reload-module cue-list-module)
  (set-playback-cue-list! pb my-cue-list)
  (cut-to-cue-number! pb
                      (get-playback-cue-number pb)))

(define (auto-reload)
  (while #t
    (sleep 1)
    (reload-cue-list)))
