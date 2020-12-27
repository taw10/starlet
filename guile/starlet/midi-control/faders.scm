(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:export (on-fader))


(define (on-fader channel cc-number state)
  (let ((fader (make-midi-controller! #:channel channel
                                      #:cc-number cc-number)))
    (register-state!
     (lighting-state
      (state-for-each
       (lambda (fix attr val)
         (if (intensity? attr)

             ;; Intensity parameters get scaled according to the fader
             (at fix attr (lambda (time)
                            (* 0.01
                               val
                               (get-controller-value fader))))

             ;; Non-intensity parameters just get set in our new state
             (at fix attr val)))

       state)))))
