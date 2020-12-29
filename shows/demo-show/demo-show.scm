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

(start-ola-output)

(start-midi-control "/dev/snd/midiC1D0")
(all-notes-off! 14)
(make-midi-playback-buttons pb 14 20 12 24 24)
(make-midi-playback-buttons pb 14 23 15 27 27)
(on-fader 14 19
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
