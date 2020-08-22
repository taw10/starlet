;; Example invokation, from top level project folder:
;; $ guile -L guile -l examples/demo.scm

(use-modules
  (starlet base)
  (starlet playback)
  (starlet effects)
  (venues demo-venue)
  (guile-midi control))

(start-ola-output)
(start-midi-control "/dev/snd/midiC1D0")
(all-notes-off! 14)


;; Set up working lights on a MIDI fader
(define working-light-fader
  (make-midi-controller! #:channel 14
                         #:cc-number 19))

(define-state worklight
  (let ((fader-pos (lambda (time)
                     (get-controller-value working-light-fader))))
    (at dim11 'intensity fader-pos)
    (at dim12 'intensity fader-pos)
    (at dim13 'intensity fader-pos)))

(register-state! worklight)


;; Same, for some different fixtures

(define movers-fader
  (make-midi-controller! #:channel 14
                         #:cc-number 18))

(define-state movers
  (let ((fader-pos (lambda (time)
                     (get-controller-value movers-fader))))
    (at mh1 'intensity fader-pos)
    (at mh2 'intensity fader-pos)))

(register-state! movers)


(define pot1
  (make-midi-controller! #:channel 14
                         #:cc-number 7))

(define-state example-state-1

  ;; Front wash
  (at dim11 'intensity 50)
  (at dim12 'intensity 50)
  (at dim13 'intensity 50)

  ;; Sidelight
  (at dim7 'intensity (flash 2))
  (at dim8 'intensity 50)

  (at dim48 'intensity
             (lambda (a)
               (get-controller-value pot1))))


(define-state example-state-2

  ;; Front wash
  (at dim1 'intensity 10)
  (at dim2 'intensity 10)
  (at dim3 'intensity 10)

  ;; Sidelight
  (at dim7 'intensity (flash 5)))


(define cue-list
  (list (cue 0 make-empty-state
             #:fade-up 1
             #:fade-down 1)

        (cue 1 (lambda () example-state-1)
             #:fade-up 3
             #:fade-down 5)

        (cue 2 (lambda () example-state-2)
             #:fade-up 3
             #:fade-down 1
             #:down-delay 3)

        (cue 3 make-empty-state
             #:fade-up 0
             #:fade-down 2)))


;; Create a playback for the cue list, and register it for output
(define pb
  (make-playback cue-list))
(register-state! pb)

;; Jump to zero (blackout) cue
(cut-to-cue-number! pb 0)

(define (make-playback-buttons chan
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
     #:func (lambda () (go! pb)))
    (register-midi-note-callback!
     #:channel chan
     #:note-number stop-button-note
     #:func (lambda () (display "Stop/back!\n")))))

(make-playback-buttons 14 20 12 24 24)
(make-playback-buttons 14 23 15 27 27)
