;; Example invokation, from top level project folder:
;; $ guile -L guile -l examples/demo.scm

(use-modules
  (starlet base)
  (starlet effects)
  (venues demo-venue)
  (guile-midi control))

(start-ola-output)
(start-midi-control "/dev/snd/midiC1D0")

(define working-light-fader
  (make-midi-controller #:channel 14
                        #:cc-number 19))

(define pot1
  (make-midi-controller #:channel 14
                        #:cc-number 7))

(define wksp (make-workspace))

(define (example-state wksp)

  (blackout wksp)

  ;; Front wash
  (set-attr! wksp dim11 'intensity
             (lambda (a)
               (get-controller-value working-light-fader)))
  (set-attr! wksp dim12 'intensity
             (lambda (a)
               (get-controller-value working-light-fader)))
  (set-attr! wksp dim13 'intensity
             (lambda (a)
               (get-controller-value working-light-fader)))

  ;; Sidelight
  (set-attr! wksp dim7 'intensity (flash 2))
  (set-attr! wksp dim8 'intensity 50)

  (set-attr! wksp dim48 'intensity
             (lambda (a)
               (get-controller-value pot1))))


(define (example2 wksp)

  (blackout wksp)

  ;; Front wash
  (set-attr! wksp dim1 'intensity 10)
  (set-attr! wksp dim2 'intensity 10)
  (set-attr! wksp dim3 'intensity 10)

  ;; Sidelight
  (set-attr! wksp dim7 'intensity (flash 5))
  (set-attr! wksp dim8 'intensity 50))

(example-state wksp)
