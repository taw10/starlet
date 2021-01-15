(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
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
              (set-attr! programmer-state
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
                (set-attr! programmer-state
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


(define-record-type <midi-control-spec>
  (make-midi-control-spec attr-name
                          type
                          cc-number
                          leds)
  midi-control-spec?
  (attr-name  attr-name)
  (type       type)
  (cc-number  cc-number)
  (leds       leds))


(define control-map
  (list
   (make-midi-control-spec 'intensity 'jogwheel 21 98)
   (make-midi-control-spec 'pan 'jogwheel 0 124)
   (make-midi-control-spec 'tilt 'jogwheel 1 125)
   (make-midi-control-spec 'r 'fader 4 '(120 84))
   (make-midi-control-spec 'g 'fader 5 '(121 85))
   (make-midi-control-spec 'b 'fader 6 '(122 86))))


(define (find-control-spec control-map needle)
  (find (lambda (a)
          (eq? (attr-name a) needle))
        control-map))


(define (midi-control-attr fixture attr-name)
  (let ((control-spec (find-control-spec
                       control-map
                       attr-name)))
    (cond

     ((not control-spec) #f)   ;; Fixture does not have this attribute

     ((eq? (type control-spec) 'jogwheel)
      (at-midi-jogwheel fixture
                        attr-name
                        (cc-number control-spec)
                        #:led (leds control-spec)))

     ((eq? (type control-spec) 'fader)
      (at-midi-fader fixture
                     attr-name
                     (cc-number control-spec)
                     #:led (car (leds control-spec))
                     #:led-incongruent (cadr (leds control-spec)))))))


;; Stuff to clear up when we're done with selected fixtures
(define midi-callbacks '())


(define (sel fixture)

  (define (merge-rule-replace attr a b) b)

  (define (led-off leds)
    (cond
     ((list? leds)
      (for-each send-note-off leds))
     ((number? leds)
      (send-note-off leds))))


  (for-each remove-midi-callback! midi-callbacks)

  (for-each (lambda (control-spec)
              (led-off (leds control-spec)))
            control-map)

  (set! midi-callbacks '())

  (when fixture
    (set! midi-callbacks
      (map (lambda (attr)
             (midi-control-attr fixture
                                (get-attr-name attr)))
           (get-attributes fixture)))))
