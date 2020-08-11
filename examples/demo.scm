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
(define worklight (make-workspace))

(define working-light-fader
  (make-midi-controller #:channel 14
                        #:cc-number 19))

(set-attr! worklight dim11 'intensity
           (lambda (a)
             (get-controller-value working-light-fader)))
(set-attr! worklight dim12 'intensity
           (lambda (a)
             (get-controller-value working-light-fader)))
(set-attr! worklight dim13 'intensity
           (lambda (a)
             (get-controller-value working-light-fader)))



;; Workspace for cue playback
(define cue-wksp (make-workspace))

(define pot1
  (make-midi-controller #:channel 14
                        #:cc-number 7))

(define (example-state-1 wksp)

  ;; Front wash
  (set-attr! wksp dim11 'intensity 100)
  (set-attr! wksp dim12 'intensity 100)
  (set-attr! wksp dim13 'intensity 100)

  ;; Sidelight
  (set-attr! wksp dim7 'intensity (flash 2))
  (set-attr! wksp dim8 'intensity 50)

  (set-attr! wksp dim48 'intensity
             (lambda (a)
               (get-controller-value pot1))))


(define (example-state-2 wksp)

  ;; Front wash
  (set-attr! wksp dim1 'intensity 10)
  (set-attr! wksp dim2 'intensity 10)
  (set-attr! wksp dim3 'intensity 10)

  ;; Sidelight
  (set-attr! wksp dim7 'intensity (flash 5))
  (set-attr! wksp dim8 'intensity 50))


(define cue-list
  (list (cue 1 example-state-1
             #:fade-up 3
             #:fade-down 5)

        (cue 2 example-state-2
             #:fade-up 3
             #:fade-down 5)))

(define pb
  (make-playback cue-list))

(cut-to-cue pb 1)
