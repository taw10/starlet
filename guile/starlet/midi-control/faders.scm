(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet base)
  #:use-module (starlet utils)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (state-on-fader))


(define (put-state-on-fader cc-number
                            channel
                            state)
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


(define* (state-on-fader cc-number state
                         #:key (channel #f))
  (register-midi-cc-callback!
    #:cc-number cc-number
    #:func (lambda (old-val new-val)
             (when (or (eqv? old-val 0)
                       (and (not old-val)
                            (< new-val 10)))
               (put-state-on-fader cc-number channel state)))))


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
                              (set-in-state! programmer-state
                                             fix
                                             attr
                                             (+ old-val offset)))
                            fixtures old-vals)))))))



(define (fader-congruent vals attrs)
  (mean (map (lambda (val attr)
               (scale-to-range val
                               (get-attr-range attr)
                               '(0 127)))
             vals attrs)))


(define (fader-up-gradients initial-vals
                            attrs
                            congruent-val)
  (map (lambda (initial-val attr)
         (let ((attr-max (cadr (get-attr-range attr))))
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
         (let ((attr-min (car (get-attr-range attr))))
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
