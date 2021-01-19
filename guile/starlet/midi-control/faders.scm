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


(define (current-values fixture-list attr-name)
  (map (lambda (fix)
         (current-value fix attr-name))
       fixture-list))


(define (fixtures-with-attr fixture-list attr-name)
  (filter (lambda (fix) (find-attr fix attr-name))
          fixture-list))


(define* (at-midi-jogwheel fixture-list attr cc-number
                           #:key (led #f))

  (define (ccval->offset a)
    (if (eq? a 127)
        -1
        1))

  (let ((fixtures (fixtures-with-attr fixture-list attr)))
    (unless (null? fixtures)

      (when led
        (send-note-on led))

      (let ((old-vals (current-values fixtures attr))
            (offset 0))
        (register-midi-cc-callback!
         #:cc-number cc-number
         #:func (lambda (prev-cc-val new-cc-value)
                  (set! offset (+ offset (ccval->offset new-cc-value)))
                  (for-each (lambda (fix old-val)
                              (set-attr! programmer-state
                                         fix
                                         attr
                                         (+ old-val offset)))
                            fixtures old-vals)))))))


(define (in-range a val1 val2)
  (or
   (and (>= a val1)
        (<= a val2))
   (and (>= a val2)
        (<= a val1))))


;; Returns a pair of (low . high), which are the amount of fader
;; space required in the downward and upward directions respectively
(define (fader-space fixtures attr-name)

  (define (attr-max-value attr)
    (cadr (get-attr-range attr)))

  (define (attr-min-value attr)
    (car (get-attr-range attr)))

  (define (distance-above-min fix attr)
    (- (current-value fix (get-attr-name attr))
       (attr-min-value attr)))

  (define (distance-below-max fix attr)
    (- (attr-max-value attr)
       (current-value fix (get-attr-name attr))))

  (fold (lambda (fix prev)
          (let ((attr (find-attr fix attr-name)))
            (cons (max (distance-above-min fix attr)
                       (car prev))
                  (max (distance-below-max fix attr)
                       (cdr prev)))))
        (cons 0 0)
        fixtures))


(define space-down car)
(define space-up cdr)

(define (space-span r)
  (+ (space-down r)
     (space-up r)))

(define (fader-space->congruence r)
  (inexact->exact
   (round
    (* 127 (/ (space-down r)
              (space-span r))))))


(define (range-scale cspace)
  (/ (+ (space-up cspace)
        (space-down cspace))
     127))


(define (conv-fader orig-cc
                    new-cc
                    initial-val
                    control-space)
  (+ initial-val
     (* (range-scale control-space)
        (- new-cc orig-cc))))


(define* (at-midi-fader fixture-list
                        attr-name
                        cc-number
                        #:key
                        (led-incongruent #f)
                        (led #f))

  (let ((fixtures (fixtures-with-attr fixture-list attr-name)))
    (unless (null? fixtures)
      (let* ((control-space (fader-space fixtures attr-name))
             (congruent-val (fader-space->congruence control-space))
             (cc-val (get-cc-value cc-number))
             (congruent (and cc-val (= cc-val congruent-val)))
             (initial-vals (current-values fixture-list attr-name)))

        (if congruent
            (send-note-on led)
            (send-note-on led-incongruent))

        (register-midi-cc-callback!
         #:cc-number cc-number
         #:func (lambda (prev-cc-val new-cc-value)

                  (when congruent
                    (for-each (lambda (fix initial-val)
                                (set-attr! programmer-state
                                           fix
                                           attr-name
                                           (conv-fader congruent-val
                                                       new-cc-value
                                                       initial-val
                                                       control-space)))
                              fixture-list
                              initial-vals))


                  (when (or (and (not prev-cc-val)
                                 (= new-cc-value congruent-val))
                            (and prev-cc-val new-cc-value
                                 (in-range congruent-val
                                           prev-cc-val
                                           new-cc-value)))
                    (set! congruent #t)
                    (send-note-on led))))))))


(define control-map
  '((intensity jogwheel 21 98)
    (pan       jogwheel 0  124)
    (tilt      jogwheel 1  125)
    (cyan      fader    4  (120 84))
    (magenta   fader    5  (121 85))
    (yellow    fader    6  (122 86))
    (cto       fader    7  (123 87))
    (iris      fader    8  (116 80))
    (zoom      fader    9  (117 81))
    (focus     fader    10 (118 82))))


(define (midi-control-attr control-spec fixture-list)
  (cond

   ((eq? 'jogwheel (cadr control-spec))
    (at-midi-jogwheel fixture-list
                      (car control-spec)
                      (caddr control-spec)
                      #:led (cadddr control-spec)))

   ((eq? 'fader (cadr control-spec))
    (at-midi-fader fixture-list
                   (car control-spec)
                   (caddr control-spec)
                   #:led (car (cadddr control-spec))
                   #:led-incongruent (cadr (cadddr control-spec))))))


;; Stuff to clear up when we're done with selected fixtures
(define midi-callbacks '())


(define (sel . fixture-list)

  (define (led-off leds)
    (cond
     ((list? leds)
      (for-each send-note-off leds))
     ((number? leds)
      (send-note-off leds))))

  (for-each remove-midi-callback! midi-callbacks)

  (for-each (lambda (control-spec)
              (led-off (cadddr control-spec)))
            control-map)

  (set! midi-callbacks '())

  (when (car fixture-list)
    (set! midi-callbacks
      (map (lambda (control-spec)
             (midi-control-attr control-spec
                                fixture-list))
           control-map))))
