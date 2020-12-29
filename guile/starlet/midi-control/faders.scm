(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:export (on-fader))


(define* (on-fader cc-number state
                   #:key (channel #f))
  (register-state!
   (lighting-state
    (state-for-each
     (lambda (fix attr val)
       (if (intensity? attr)

           ;; Intensity parameters get scaled according to the fader
           (at fix attr (lambda (time)
                          (* 0.01
                             val
                             (scale-127-100 (get-cc-value cc-number
                                                          #:channel channel)))))

           ;; Non-intensity parameters just get set in our new state
           (at fix attr val)))

     state))))
