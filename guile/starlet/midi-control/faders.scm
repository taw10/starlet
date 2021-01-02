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
                              (let ((cc-val (get-cc-value cc-number
                                                          #:channel channel)))
                                (if cc-val
                                  (* 0.01 val (ccval->percent cc-val))
                                  0))))

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


(define (in-range a val1 val2)
  (or
   (and (>= a val1)
        (<= a val2))
   (and (>= a val2)
        (<= a val1))))


(define* (at-midi-fader fix
                        attr
                        cc-number
                        #:key
                        (led-incongruent #f)
                        (led #f))

  (let* ((congruent-val (percent->ccval (current-value fix attr)))

         (cc-val (get-cc-value cc-number))
         (congruent (and cc-val
                         (= cc-val congruent-val))))

    (if congruent
        (send-note-on led)
        (send-note-on led-incongruent))

    (register-midi-cc-callback!
     #:cc-number cc-number
     #:func (lambda (prev-cc-val new-cc-value)

              (when congruent
                (set-attr! selection-state
                           fix
                           attr
                           (ccval->percent new-cc-value)))

              (when (or (and (not prev-cc-val)
                             (= new-cc-value congruent-val))
                        (and prev-cc-val new-cc-value
                             (in-range congruent-val
                                       prev-cc-val
                                       new-cc-value)))
                (set! congruent #t)
                (send-note-on led))))))


(define (select-fixtures fixture)
  (values
   (list 98 124 125 84 85 86 120 121 122)
   (list
    (at-midi-jogwheel fixture 'intensity 21
                      #:led 98)
    (at-midi-jogwheel fixture 'pan 0
                      #:led 124)
    (at-midi-jogwheel fixture 'tilt 1
                      #:led 125)
    (at-midi-fader fixture 'r 4
                      #:led 120
                      #:led-incongruent 84)
    (at-midi-fader fixture 'g 5
                      #:led 121
                      #:led-incongruent 85)
    (at-midi-fader fixture 'b 6
                      #:led 122
                      #:led-incongruent 86))))


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
