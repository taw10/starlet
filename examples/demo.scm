;; Example invokation, from top level project folder:
;; $ guile -L guile -l examples/demo.scm

(use-modules
  (starlet base)
  (starlet effects)
  (venues demo-venue)
  (guile-midi control))

(start-ola-output)
(start-midi-control "/dev/snd/midiC1D0")


;; Light up some LEDs on the MIDI controller
(define led1
  (make-midi-led #:channel 14
                 #:note-number 23))

(define led2
  (make-midi-led #:channel 14
                 #:note-number 20))

(set-midi-led! led1 #t)
(set-midi-led! led2 #t)


;; Set up working lights on a MIDI fader
(define working-light-fader
  (make-midi-controller #:channel 14
                        #:cc-number 19))

(define (worklight)
  (let ((state (make-empty-state))
        (fader-pos (get-controller-value working-light-fader)))
    (set-attr! state dim11 'intensity fader-pos)
    (set-attr! state dim12 'intensity fader-pos)
    (set-attr! state dim13 'intensity fader-pos)
    state))

(register-state! worklight)


(define pot1
  (make-midi-controller #:channel 14
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
  (list (cue 1 example-state-1
             #:fade-up 3
             #:fade-down 5)

        (cue 2 example-state-2
             #:fade-up 3
             #:fade-down 5)))

(define pb
  (make-playback cue-list))

(register-state! pb)

(cut-to-cue pb 1)
