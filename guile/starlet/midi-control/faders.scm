(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:use-module (ice-9 receive)
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


(define* (at-midi-jogwheel fix attr cc-number
                           #:key (led #f))

  (define (ccval->offset a)
    (if (eq? a 127)
        -1
        1))

  (when led
    (send-note-on led))

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
  (values
   (list 98 124 125 84 85 86 120 121 122)
   (list
    (at-midi-jogwheel fixture 'intensity 21
                      #:led 98)
    (at-midi-jogwheel fixture 'pan 0
                      #:led 124)
    (at-midi-jogwheel fixture 'tilt 1
                      #:led 125))))
;;    (at-midi-fader fixture 'red 4
;;                      #:led-incongruent 84
;;                      #:led-congruent 120)
;;    (at-midi-fader fixture 'green 5
;;                      #:led-incongruent 85
;;                      #:led-congruent 121)
;;    (at-midi-fader fixture 'blue 6
;;                      #:led-incongruent 86
;;                      #:led-congruent 122))))


;; Stuff to clear up when we're done with selected fixtures
(define midi-callbacks '())
(define midi-leds '())


(define (sel fixture)

  (define (merge-rule-replace attr a b) b)

  (when selection-state
    (add-state-to-state! merge-rule-replace
                         selection-state
                         programmer-state)
    (clear-state! selection-state)
    (for-each remove-midi-callback! midi-callbacks)
    (for-each send-note-off midi-leds)
    (set! midi-callbacks '())
    (set! midi-leds '()))

  (when fixture
    (receive (leds callbacks)
      (select-fixtures fixture)
      (set! midi-callbacks callbacks)
      (set! midi-leds leds))))
