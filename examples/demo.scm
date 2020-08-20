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

(define (worklight)
  (let ((state (make-empty-state))
        (fader-pos (get-controller-value working-light-fader)))
    (set-attr! state dim11 'intensity fader-pos)
    (set-attr! state dim12 'intensity fader-pos)
    (set-attr! state dim13 'intensity fader-pos)
    state))

(register-state! worklight)


(define movers-fader
  (make-midi-controller! #:channel 14
                         #:cc-number 18))
(define (movers)
  (let ((state (make-empty-state))
        (fader-pos (get-controller-value movers-fader)))
    (set-attr! state mh1 'intensity fader-pos)
    (set-attr! state mh2 'intensity fader-pos)
    state))

(register-state! movers)


(define pot1
  (make-midi-controller! #:channel 14
                         #:cc-number 7))

(define (example-state-1)

  (let ((state (make-empty-state)))

    ;; Front wash
    (set-attr! state dim11 'intensity 50)
    (set-attr! state dim12 'intensity 50)
    (set-attr! state dim13 'intensity 50)

    ;; Sidelight
    (set-attr! state dim7 'intensity (flash 2))
    (set-attr! state dim8 'intensity 50)

    (set-attr! state dim48 'intensity
               (lambda (a)
                 (get-controller-value pot1)))

    state))


(define (example-state-2)

  (let ((state (make-empty-state)))

    ;; Front wash
    (set-attr! state dim1 'intensity 10)
    (set-attr! state dim2 'intensity 10)
    (set-attr! state dim3 'intensity 10)

    ;; Sidelight
    (set-attr! state dim7 'intensity (flash 5))

    state))


(define cue-list
  (list (cue 0 (make-empty-state)
             #:fade-up 1
             #:fade-down 1)

        (cue 1 example-state-1
             #:fade-up 3
             #:fade-down 5)

        (cue 2 example-state-2
             #:fade-up 3
             #:fade-down 1
             #:down-delay 3)

        (cue 3 (make-empty-state)
             #:fade-up 0
             #:fade-down 2)))


;; Create a playback for the cue list, and register it for output
(define pb
  (make-playback cue-list))
(register-state! pb)

;; Jump to zero (blackout) cue
(cut-to-cue-number! pb 0)

;; Left-hand playback buttons
(define go1 (make-midi-led #:channel 14 #:note-number 20))
(set-midi-led! go1 #t)
(define stop1 (make-midi-led #:channel 14 #:note-number 24))
(set-midi-led! stop1 #t)
(register-midi-note-callback!
 #:channel 14
 #:note-number 12
 #:func (lambda () (go! pb)))
(register-midi-note-callback!
 #:channel 14
 #:note-number 24
 #:func (lambda () (display "Stop/back!\n")))

;; Right-hand playback buttons
(define go2 (make-midi-led #:channel 14 #:note-number 23))
(set-midi-led! go2 #t)
(define stop2 (make-midi-led #:channel 14 #:note-number 27))
(set-midi-led! stop2 #t)
(register-midi-note-callback!
 #:channel 14
 #:note-number 15
 #:func (lambda () (go! pb)))
(register-midi-note-callback!
 #:channel 14
 #:note-number 27
 #:func (lambda () (display "Stop/back!\n")))
