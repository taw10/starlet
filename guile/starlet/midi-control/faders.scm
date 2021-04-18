(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:use-module (starlet colours)
  #:use-module (starlet utils)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (state-on-fader))


(define (channel-number->string channel)
  (if channel
      (number->string channel)
      "default"))


(define (name-for-fader-state channel cc-number)
  (string->symbol
    (string-append
      "faderstate-ch"
      (channel-number->string channel)
      "-cc"
      (number->string cc-number))))


(define* (state-on-fader cc-number
                         state
                         #:key (channel #f))
  (register-state!
    (lighting-state
      (state-for-each
        (lambda (fix attr val)
          (at fix attr
              (lambda (time)

                (let ((cc-val (get-cc-value cc-number #:channel channel)))

                  ;; Fader position known?
                  (if cc-val

                      (if (intensity? attr)

                          ;; Intensity parameters get scaled according to the fader
                          (* 0.01 val (ccval->percent cc-val))

                          ;; Non-intensity parameters just get set in our new state,
                          ;; but only if the fader is up!
                          (if (> cc-val 0)
                              val
                              'no-value))

                      ;; Fader position unknown
                      'no-value)))))

        state))
    #:unique-name (name-for-fader-state channel cc-number)))


(define (current-values fixture-list attr-name)
  (map (lambda (fix)
         (current-value fix attr-name (hirestime)))
       fixture-list))


(define (fixtures-with-attr fixture-list attr-name)
  (let ((attrs (map (partial find-attr attr-name) fixture-list)))
    (fold (lambda (fix attr old)
            (if attr
              (cons (cons fix (car old))
                    (cons attr (cdr old)))
              old))
          (cons '() '())
          fixture-list attrs)))

(define (clamp-to-attr-range attr-obj val)
  (let ((r (get-attr-range-maybe-colour attr-obj)))
    (max (car r)
         (min (cadr r)
              val))))

(define* (at-midi-jogwheel fixture-list attr cc-number
                           #:key (led #f))

  (define (ccval->offset a)
    (if (eq? a 127)
        -1
        1))

  (let ((fixtures (car (fixtures-with-attr fixture-list attr))))
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
                               (let ((attr-obj (find-attr fix attr)))
                                 (when (and attr-obj
                                            (continuous-attribute? attr-obj))
                                   (set-in-state! programmer-state
                                                  fix
                                                  attr
                                                  (clamp-to-attr-range
                                                    attr-obj
                                                    (+ old-val offset))))))
                             fixtures old-vals)))))))


(define (get-attr-range-maybe-colour attr-obj)
  (if (colour-attribute? attr-obj)
      '(0 100)
      (get-attr-range attr-obj)))


(define (fader-congruent vals attrs)
  (mean (map (lambda (val attr)
               (scale-to-range val
                               (get-attr-range-maybe-colour attr)
                               '(0 127)))
             vals attrs)))


(define (fader-up-gradients initial-vals
                            attrs
                            congruent-val)
  (map (lambda (initial-val attr)
         (let ((attr-max (cadr (get-attr-range-maybe-colour attr))))
           (if (< congruent-val 127)
               (/ (- attr-max initial-val)
                  (- 127 congruent-val))
               0)))
       initial-vals
       attrs))


(define (fader-down-gradients initial-vals
                              attrs
                              congruent-val)
  (map (lambda (initial-val attr)
         (let ((attr-min (car (get-attr-range-maybe-colour attr))))
           (if (> congruent-val 0)
               (/ (- initial-val attr-min)
                  congruent-val)
               0)))

       initial-vals
       attrs))


(define (apply-fader cc-offset
                     attr-name
                     gradients
                     initial-vals
                     fixtures)
  (for-each (lambda (fix initial-val gradient)
              (set-in-state! programmer-state
                             fix
                             attr-name
                             (+ initial-val
                                (* gradient cc-offset))))
            fixtures
            initial-vals
            gradients))


(define* (at-midi-fader fixture-list
                        attr-name
                        cc-number
                        #:key
                        (led-incongruent #f)
                        (led #f))

  (let ((fixtures-attrs (fixtures-with-attr fixture-list attr-name)))
    (unless (null? (car fixtures-attrs))
      (let* ((fixtures (car fixtures-attrs))
             (attrs (cdr fixtures-attrs))
             (initial-vals (current-values fixtures attr-name))
             (congruent-val (fader-congruent initial-vals attrs))
             (up-gradients (fader-up-gradients initial-vals attrs congruent-val))
             (dn-gradients (fader-down-gradients initial-vals attrs congruent-val))
             (cc-val (get-cc-value cc-number))
             (congruent (and cc-val (= cc-val congruent-val))))

        (if congruent
            (send-note-on led)
            (send-note-on led-incongruent))

        (register-midi-cc-callback!
          #:cc-number cc-number
          #:func (lambda (prev-cc-val new-cc-value)

                   (if congruent

                       (cond
                         ((> new-cc-value congruent-val)
                          (apply-fader (- new-cc-value congruent-val)
                                       attr-name
                                       up-gradients
                                       initial-vals
                                       fixtures))
                         ((< new-cc-value congruent-val)
                          (apply-fader (- new-cc-value congruent-val)
                                       attr-name
                                       dn-gradients
                                       initial-vals
                                       fixtures)))

                       (when (or (and (not prev-cc-val)
                                      (= new-cc-value congruent-val))
                                 (and prev-cc-val new-cc-value
                                      (in-range congruent-val
                                                prev-cc-val
                                                new-cc-value)))
                         (set! congruent #t)
                         (send-note-on led)))))))))


(define control-map
  (list
    (list 'intensity                     'fader    16 '(72 72))
    (list 'pan                           'jogwheel 0  124)
    (list 'tilt                          'jogwheel 1  125)
    (list (colour-component-id 'cyan)    'fader    4  '(120 84))
    (list (colour-component-id 'magenta) 'fader    5  '(121 85))
    (list (colour-component-id 'yellow)  'fader    6  '(122 86))
    (list 'cto                           'fader    7  '(123 87))
    (list 'iris                          'fader    8  '(116 80))
    (list 'zoom                          'fader    9  '(117 81))
    (list 'focus                         'fader    10 '(118 82))))


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


(define (select-midi fixture-list)

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

  (unless (nil? fixture-list)
    (set! midi-callbacks
      (map (partial midi-control-attr fixture-list)
           control-map))))


(add-hook! selection-hook select-midi)
