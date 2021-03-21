(define-module (starlet playback)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (starlet base)
  #:use-module (starlet utils)
  #:export (make-playback
            cue
            cue-part
            cut-to-cue-number!
            get-playback-cue-number
            run-cue-number!
            go!
            cue-list
            set-playback-cue-list!))


;; A "playback" is a state which knows how to run cues
;; from a cue list
(define-class <starlet-playback> (<starlet-state>)
  (cue-list
   #:init-keyword #:cue-list
   #:getter get-playback-cue-list
   #:setter set-playback-cue-list!)

  (next-cue-index
   #:init-value 0
   #:getter get-next-cue-index
   #:setter set-next-cue-index!)

  (fade-records
   #:init-form (make-hash-table)
   #:getter get-fade-records
   #:setter set-fade-records!))


(define-record-type <cue-part>
  (make-cue-part attr-list
                 fade-times)
  cue-part?
  (attr-list    get-cue-part-attr-list)
  (fade-times   get-cue-part-fade-times))


(define-record-type <fade-times>
  (make-fade-times up-time
                   down-time
                   attr-time
                   up-delay
                   down-delay
                   attr-delay
                   preset-time
                   preset-delay)
  fade-times?
  (up-time      get-fade-up-time)
  (down-time    get-fade-down-time)
  (attr-time    get-fade-attr-time)
  (up-delay     get-fade-up-delay)
  (down-delay   get-fade-down-delay)
  (attr-delay   get-fade-attr-delay)
  (preset-time  get-fade-preset-time)
  (preset-delay get-fade-preset-delay))


;; Macro to avoid a profusion of (get-fade-xxx-time fade-times)
(define-syntax with-fade-times
  (lambda (x)
    (syntax-case x ()
      ((_ fade-times body ...)
       (with-syntax ((up-time (datum->syntax x 'up-time))
                     (down-time (datum->syntax x 'down-time))
                     (attr-time (datum->syntax x 'attr-time))
                     (up-delay (datum->syntax x 'up-delay))
                     (down-delay (datum->syntax x 'down-delay))
                     (attr-delay (datum->syntax x 'attr-delay))
                     (preset-time (datum->syntax x 'preset-time))
                     (preset-delay (datum->syntax x 'preset-delay)))
         #'(let ((up-time (get-fade-up-time fade-times))
                 (down-time (get-fade-down-time fade-times))
                 (attr-time (get-fade-attr-time fade-times))
                 (up-delay (get-fade-up-delay fade-times))
                 (down-delay (get-fade-down-delay fade-times))
                 (attr-delay (get-fade-attr-delay fade-times))
                 (preset-time (get-fade-preset-time fade-times))
                 (preset-delay (get-fade-preset-delay fade-times)))
             body ...))))))


(define-record-type <fade-record>
  (make-fade-record start-time
                    fade-times
                    previous
                    target
                    preset)
  fade-record?
  (start-time         fade-start-time)
  (fade-times         get-fade-record-fade-times)
  (previous           fade-previous)
  (target             fade-target)
  (preset             fade-preset))


(define-record-type <cue>
  (make-cue number
            state
            tracked-state
            fade-times
            track-intensities
            cue-parts)
  cue?
  (number             get-cue-number)
  (state              get-cue-state)
  (tracked-state      get-tracked-state
                      set-tracked-state!)
  (fade-times         get-cue-fade-times)
  (track-intensities  track-intensities)
  (cue-parts          get-cue-parts))


(define (get-playback-cue-number pb)
  (cue-index-to-number (get-playback-cue-list pb)
                       (max 0 (- (get-next-cue-index pb) 1))))

(define (qnum a)
  (/ (inexact->exact (* a 1000)) 1000))


(define (make-playback cue-list)
  (let ((new-playback (make <starlet-playback>
                        #:cue-list cue-list)))
    (register-state! new-playback)
    new-playback))


(define (cue-index-to-number cue-list cue-index)
  (get-cue-number (vector-ref cue-list cue-index)))


(define (cue-number-to-index cue-list cue-number)
  (vector-index (lambda (a)
                  (eqv? (get-cue-number a)
                        cue-number))
                cue-list))


(define (cut-to-cue-number! pb cue-number)

  (let* ((cue-list (get-playback-cue-list pb))
         (cue-index (cue-number-to-index cue-list (qnum cue-number))))

    (set-state-hash-table! pb (copy-hash-table
                                (get-state-hash-table
                                  (calculate-tracking cue-list cue-index))))
    (set-next-cue-index! pb (+ cue-index 1))

    ;; Wipe out the old fade params
    (set-fade-records! pb (make-hash-table))

    ;; Record fade params
    (state-for-each
      (lambda (fix attr val)
        (let ((new-record (make-fade-record (hirestime)
                                            (make-fade-times
                                              0.0
                                              0.0
                                              0.0
                                              0.0
                                              0.0
                                              0.0
                                              0.0
                                              0.0)
                                            0.0
                                            val
                                            (preset-val cue-list
                                                        cue-index
                                                        fix
                                                        attr))))
          (hash-set! (get-fade-records pb)
                     (cons fix attr)
                     new-record)
          (apply-fade-record pb fix attr new-record)))
      pb))

  *unspecified*)


(define (go! pb)
  (let ((cue-index (get-next-cue-index pb)))
    (run-cue! pb cue-index))
  *unspecified*)


(define (run-cue-number! pb cue-number)
  (let ((cue-index (cue-number-to-index (get-playback-cue-list pb)
                                        cue-number)))
    (run-cue! pb cue-index))
  *unspecified*)


(define (run-cue! pb cue-index)
  (let* ((cue-list (get-playback-cue-list pb)))
    (unless (>= cue-index (vector-length cue-list))
      (run-cue-index! pb cue-list cue-index (hirestime))
      (set-next-cue-index! pb (+ cue-index 1))))
      ;; else at the end of the cue list
  *unspecified*)


(define (fade-func start-val
                   end-val
                   preset-val
                   fade-time
                   delay-time
                   preset-time
                   preset-delay
                   start-time
                   current-time)
  (let ((elapsed-fade-time (- current-time start-time delay-time)))
    (cond

      ;; Before start of fade
      ((< elapsed-fade-time 0)
       start-val)

      ;; After both fade and preset fade
      ((and (not (eq? preset-val 'attribute-not-in-state))
            (> elapsed-fade-time (+ fade-time preset-delay preset-time)))
       preset-val)

      ;; During preset fade
      ((and (not (eq? preset-val 'attribute-not-in-state))
            (> elapsed-fade-time (+ preset-delay fade-time)))
       (+ end-val
          (* (- preset-val end-val)
             (/ (- elapsed-fade-time fade-time preset-delay)
                preset-time))))

      ;; After end of fade, but not long enough for auto-move
      ((> elapsed-fade-time fade-time)
       end-val)

      ;; During the fade
      (else
        (+ start-val
           (* (- end-val start-val)
              ;; Fraction of fade time elapsed
              (/ elapsed-fade-time fade-time)))))))


;; Return a function to fade from start-val to end-val using the
;; specified fade time and delay, starting at tnow
(define (wrap-fade start-val
                   end-val
                   preset-val
                   fade-time
                   delay-time
                   preset-time
                   preset-delay
                   tnow)
  (lambda (time)
    (fade-func (value->number start-val time)
               (value->number end-val time)
               (value->number preset-val time)
               fade-time
               delay-time
               preset-time
               preset-delay
               tnow
               time)))


;; Return a function for HTP mix of:
;;    start-val fading down in down-time/down-delay
;;    end-val fading up in up-time/up-delay
(define (wrap-xf start-val
                 end-val
                 fade-times
                 tnow)
  (with-fade-times
   fade-times
   (lambda (time)
     (max
      (fade-func (value->number start-val time)
                 0
                 #f            ;; wrap-xf is only used for intensities,
                 down-time     ;; so auto-move/preset is irrelevant
                 down-delay
                 0
                 0
                 tnow
                 time)
      (fade-func 0
                 (value->number end-val time)
                 #f
                 up-time
                 up-delay
                 0
                 0
                 tnow
                 time)))))


(define (fade-start-val tnow pb old-fade-record fix attr)
  (cond

   ;; Attr not seen before in this playback: start fading from home
   ((eq? old-fade-record 'attribute-not-in-state)
    (get-attr-home-val fix attr))

   ;; Attr seen in a finished fade
   ((fade-finished? tnow old-fade-record)
    (or (fade-preset old-fade-record)
        (fade-target old-fade-record)))

   ;; Attr is currently fading: get the current state
   ;; (NB it might be a function/effect)
   (else
    (let ((func (state-find fix attr pb)))
      (func tnow)))))


;; Work out the fade function for fade-record, and apply it to pb
(define (apply-fade-record pb fix attr fade-record)
  (with-fade-times
    (get-fade-record-fade-times fade-record)
    (let ((prev-val (fade-previous fade-record))
          (target-val (fade-target fade-record))
          (preset-val (fade-preset fade-record)))

      (cond

        ((eq? target-val 'attribute-not-in-state)
         (display "Target val not defined\n"))

        ;; FIXME: Attributes fading to/from attribute-not-in-state
        ;;    ... depends on intensity vs non-intensity?
        ;;    ... depends on what intensity of fixture is doing?

        ;; Non-intensity attribute
        ((not (intensity? attr))
         (set-in-state! pb fix attr (wrap-fade prev-val
                                               target-val
                                               preset-val
                                               attr-time
                                               attr-delay
                                               preset-time
                                               preset-delay
                                               (fade-start-time fade-record))))

        ;; Number to number, fading up
        ((and (number? target-val)
              (number? prev-val)
              (> target-val prev-val))
         (set-in-state! pb fix attr (wrap-fade prev-val
                                               target-val
                                               #f
                                               up-time
                                               up-delay
                                               0.0
                                               0.0
                                               (fade-start-time fade-record))))

        ;; Number to number, fading down
        ((and (number? target-val)
              (number? prev-val)
              (< target-val prev-val))
         (set-in-state! pb fix attr (wrap-fade prev-val
                                               target-val
                                               #f
                                               down-time
                                               down-delay
                                               0.0
                                               0.0
                                               (fade-start-time fade-record))))

        ;; Number to number, staying the same
        ((and (number? target-val)
              (number? prev-val))
         (set-in-state! pb fix attr (wrap-fade prev-val
                                               target-val
                                               #f
                                               0.0
                                               0.0
                                               0.0
                                               0.0
                                               (fade-start-time fade-record))))

        ;; Everything else, e.g. number to effect
        (else
          (set-in-state! pb fix attr (wrap-xf prev-val
                                              target-val
                                              (get-fade-record-fade-times fade-record)
                                              (fade-start-time fade-record))))))))


(define (fade-finished? tnow fade-record)
  (with-fade-times
   (get-fade-record-fade-times fade-record)
   (and
    (> tnow
       (+ (fade-start-time fade-record)
          up-delay
          up-time))
    (> tnow
       (+ (fade-start-time fade-record)
          down-delay
          down-time))
    (> tnow
       (+ (fade-start-time fade-record)
          attr-delay
          attr-time
          (if (fade-preset fade-record)
              (+ preset-time preset-delay)
              0))))))


(define (match-fix-attr attr-el fix attr)
  (cond

   ((fixture? attr-el)
    (eq? attr-el fix))

   ((and (pair? attr-el)
         (fixture? (car attr-el))
         (symbol? (cdr attr-el)))
    (and (eq? (car attr-el) fix)
         (eq? (cdr attr-el) attr)))

   ((list? attr-el)
    (and (memq fix attr-el)
         (memq attr attr-el)))

   (else #f)))


(define (in-cue-part? cue-part fix attr)
  (find (lambda (p) (match-fix-attr p fix attr))
        (get-cue-part-attr-list cue-part)))


(define (cue-part-fade-times the-cue fix attr)

  (let ((the-cue-part
         (find (lambda (p) (in-cue-part? p fix attr))
               (get-cue-parts the-cue))))

    (if (cue-part? the-cue-part)
      (get-cue-part-fade-times the-cue-part)
      (get-cue-fade-times the-cue))))


(define (fixture-dark? fix the-cue)
  (let ((val (state-find fix
                         'intensity
                         (get-tracked-state the-cue))))
    (or (not (have-value val))
        (eqv? 0 val))))


(define (next-value cue-list cue-index fix attr)
  (if (>= cue-index (- (vector-length cue-list) 1))
      #f
      (let ((the-cue-state (calculate-tracking cue-list (+ 1 cue-index))))
        (state-find fix
                    attr
                    the-cue-state))))


(define (preset-val cue-list cue-index fix attr)
  (let ((the-cue (vector-ref cue-list cue-index)))
    (if (fixture-dark? fix the-cue)
        (next-value cue-list cue-index fix attr)
        #f)))


(define (fix-attr-eq fa1 fa2)
  (and (eq? (car fa1) (car fa2))
       (eq? (cdr fa1) (cdr fa2))))


(define (hash-map-keys hm)
  (hash-map->list (lambda (key val) key)
                  hm))

(define (add-fix-attrs-to-list state old-list)
  (lset-union fix-attr-eq
              old-list
              (hash-map-keys
                (get-state-hash-table state))))


(define (fix-attrs-involved . states)
  (fold add-fix-attrs-to-list
        '()
        states))


(define (run-cue-index! pb cue-list cue-index tnow)

  (let ((the-cue-state (calculate-tracking cue-list cue-index))
        (next-cue-state (calculate-tracking cue-list (+ cue-index 1)))
        (the-cue (vector-ref cue-list cue-index)))

    (for-each
      (lambda (fix-attr)

        (let* ((fix (car fix-attr))
               (attr (cdr fix-attr))
               (fade-record (hash-ref (get-fade-records pb) fix-attr))
               (start-val (fade-start-val tnow pb fade-record fix attr))
               (target-val (state-find fix attr the-cue-state))
               (preset-val (state-find fix attr next-cue-state)))

          (let ((new-record (make-fade-record tnow
                                              (cue-part-fade-times the-cue
                                                                   fix
                                                                   attr)
                                              start-val
                                              target-val
                                              preset-val)))

            (hash-set! (get-fade-records pb)
                       fix-attr
                       new-record)

            (apply-fade-record pb
                               fix
                               attr
                               new-record))))

      (fix-attrs-involved pb
                          the-cue-state
                          next-cue-state))))


;;; ******************** Cue lists ********************

(define-syntax cue-part
  (syntax-rules ()
    ((_ (fixtures ...) params ...)
     (make-cue-part-obj (list fixtures ...)
                        params ...))))


(define* (make-cue-part-obj attr-list
                            #:key
                            (up-time 5)
                            (down-time 5)
                            (attr-time 3)
                            (up-delay 0)
                            (down-delay 0)
                            (attr-delay 0)
                            (preset-time 1)
                            (preset-delay 1))
  (make-cue-part attr-list
                 (make-fade-times
                  up-time
                  down-time
                  attr-time
                  up-delay
                  down-delay
                  attr-delay
                  preset-time
                  preset-delay)))


(define cue
  (lambda (number state . rest)
    (receive (cue-parts rest-minus-cue-parts)
        (partition cue-part? rest)
      (let-keywords rest-minus-cue-parts #f
                    ((up-time 5)
                     (down-time 5)
                     (attr-time 5)
                     (up-delay 0)
                     (down-delay 0)
                     (attr-delay 0)
                     (preset-time 1)
                     (preset-delay 1)
                     (track-intensities #f))

                    (make-cue (qnum number)
                              state
                              #f
                              (make-fade-times
                               up-time
                               down-time
                               attr-time
                               up-delay
                               down-delay
                               attr-delay
                               preset-time
                               preset-delay)
                              track-intensities
                              cue-parts)))))


(define (ensure-cue-zero-realized cue-list)
  (let ((cue-zero (vector-ref cue-list 0)))
    (unless (get-tracked-state cue-zero)
      (parameterize ((current-state (make-empty-state)))
        (set-tracked-state! cue-zero (current-state))))))


;; Get the state for a cue, taking into account tracking etc
(define (calculate-tracking cue-list cue-index)

  (ensure-cue-zero-realized cue-list)

      (let* ((the-cue (vector-ref cue-list cue-index))
             (rstate (get-tracked-state the-cue)))
        (or rstate
            (let ((previous-state (calculate-tracking cue-list (- cue-index 1))))
              (parameterize ((current-state (make-empty-state)))
                (apply-state previous-state)
                (unless (track-intensities the-cue)
                  (blackout (current-state)))
                (apply-state (get-cue-state the-cue))
                (set-tracked-state! the-cue (current-state))
                (current-state))))))


(define-syntax cue-list
  (syntax-rules ()
    ((_ body ...)
     (vector (cue 0
                  (make-empty-state)
                  #:up-time 0
                  #:down-time 0
                  #:attr-time 0
                  #:preset-time 0
                  #:preset-delay 0)
             body ...))))
