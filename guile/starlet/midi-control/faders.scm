(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:export (on-fader
            sel))


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


(define (at-midi-jogwheel fix attr cc-number)

  (define (ccval->offset a)
    (if (eq? a 127)
        -1
        1))

  (let ((old-val (current-value fix attr))
        (offset 0))
    (register-midi-cc-callback!
     #:cc-number cc-number
     #:func (lambda (prev-cc-val new-cc-value)
              (set! offset (+ offset (ccval->offset new-cc-value)))
              (set-attr! selection-state
                         fix
                         attr
                         (+ old-val offset))))))


(define (select-fixtures fixture)
  (list
   (at-midi-jogwheel fixture 'intensity 21)
   (at-midi-jogwheel fixture 'pan 0)
   (at-midi-jogwheel fixture 'tilt 1)))


(define midi-callbacks '())


(define (sel fixture)

  (define (merge-rule-replace attr a b) b)

  (when selection-state
    (add-state-to-state! merge-rule-replace
                         selection-state
                         programmer-state)
    (clear-state! selection-state)
    (for-each remove-midi-callback! midi-callbacks))
  (when fixture
    (set! midi-callbacks
      (select-fixtures fixture))))
