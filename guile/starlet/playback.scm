(define-module (starlet playback)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 exceptions)
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
            set-playback-cue-list!
            print-playback))


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
   #:setter set-next-cue-index!))


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

    (unless cue-index
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Invalid cue number")
                         (make-exception-with-irritants
                           (list pb cue-number)))))

    (clear-state! pb)
    (set-next-cue-index! pb (+ cue-index 1))
    (let ((cue-state (calculate-tracking cue-list cue-index)))
      (state-for-each
        (lambda (fix attr val)
          (set-in-state! pb fix attr (lambda (time) val)))
        cue-state))

    *unspecified*))


(define (run-cue-number! pb cue-number)

  (let* ((cue-list (get-playback-cue-list pb))
         (cue-index (cue-number-to-index cue-list (qnum cue-number))))

    (unless cue-index
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Invalid cue number")
                         (make-exception-with-irritants
                           (list pb cue-number)))))

    (set-next-cue-index! pb (+ cue-index 1))
    (run-cue-index! pb cue-index)
    *unspecified*))


(define (go! pb)
  (let ((next-cue-index (get-next-cue-index pb)))
    (if (< next-cue-index (vector-length (get-playback-cue-list pb)))
        (begin
          (run-cue-index! pb next-cue-index)
          (set-next-cue-index! pb (+ next-cue-index 1))
          *unspecified*)
        'no-more-cues-in-list)))


(define (simple-fade start-val
                     end-val
                     fade-time
                     start-time
                     current-time)

  (unless (and (number? start-val)
               (number? end-val))
    (raise-exception (make-exception
                       (make-exception-with-message
                         "Non-number arguments given to simple-fade")
                       (make-exception-with-irritants
                         (list start-val end-val)))))

  (let ((elapsed-fade-time (- current-time start-time)))
    (cond

      ;; Before start of fade
      ((< elapsed-fade-time 0)
       start-val)

      ;; After fade
      ((> elapsed-fade-time fade-time)
       end-val)

      ;; During the fade
      (else
        (+ start-val
           (* (- end-val start-val)
              ;; Fraction of fade time elapsed
              (/ elapsed-fade-time fade-time)))))))


;; Return a function to fade from start-val to end-val using the
;; specified fade time and delay, starting at the specified time
(define (make-fade start-val
                   end-val
                   fade-time
                   fade-start-time)
  (lambda (time)
    (simple-fade (value->number start-val time)
                 (value->number end-val time)
                 fade-time
                 fade-start-time
                 time)))


;; Return a function for HTP mix of:
;;    start-val fading down in down-time/down-delay
;;    end-val fading up in up-time/up-delay
(define (make-xf start-val
                 end-val
                 fade-times
                 fade-start-time)
  (with-fade-times fade-times
   (lambda (time)
     (max
      (simple-fade (value->number start-val time)
                   0
                   down-time
                   fade-start-time
                   time)
      (simple-fade 0
                   (value->number end-val time)
                   up-time
                   fade-start-time
                   time)))))


(define (replace-noval val replacement)
  (if (eq? 'no-value val) replacement val))


(define (make-intensity-fade prev-val target-val-in fade-times fade-start-time)
  (with-fade-times
    fade-times

    ;; Since we only handle intensities here, "not in state" should be
    ;; interpreted as zero intensity.
    (let ((target-val (replace-noval target-val-in 0.0)))

      (cond

        ;; Number to number, fading up
        ((and (number? target-val)
              (number? prev-val)
              (> target-val prev-val))
         (make-fade prev-val
                    target-val
                    up-time
                    (+ fade-start-time up-delay)))

        ;; Number to number, fading down
        ((and (number? target-val)
              (number? prev-val)
              (< target-val prev-val))
         (make-fade prev-val
                    target-val
                    down-time
                    (+ fade-start-time down-delay)))

        ;; Number to number, staying the same
        ;; NB We still need a static value so that fade-start-val can "unwrap" it
        ((and (number? target-val)
              (number? prev-val))
         (lambda (time) prev-val))

        ;; Everything else, e.g. number to effect
        (else
          (make-xf prev-val
                   target-val
                   fade-times
                   fade-start-time))))))


(define (make-list-attr-fade start-val
                             target-val
                             preset-val
                             fade-time
                             fade-start-time
                             preset-time
                             preset-start-time)
  (lambda (time)
    (cond
      ((< time fade-start-time) start-val)
      ((> time (+ preset-start-time preset-time)) preset-val)
      (else target-val))))


(define (make-continuous-attr-fade start-val
                                   target-val
                                   preset-val
                                   fade-times
                                   fade-start-time
                                   preset-time
                                   preset-start-time)
  (lambda (time)

    ;; FIXME: If target value is a number, do a fade
    ;;  (if starting value is a function, freeze it)
    (cond

      ((< time fade-start-time) start-val)

      ((and (not (eq? 'no-value preset-val))
            (> time preset-start-time))
       preset-val)

      (else target-val))))


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


(define (fade-start-val tnow pb fix attr)
  (let ((val-in-pb (state-find fix attr pb)))
    (if (eq? val-in-pb 'no-value)

        ;; Not currently in playback - fade from home value
        (get-attr-home-val fix attr)

        ;; Currently in playback - fade from current value
        ;; by running the outer crossfade function
        (val-in-pb tnow))))


(define (dark? a)
  (or (eq? a 'no-value)
      (and (number? a)
           (< a 1))))


;; NB next-cue-state might be #f, if there is no next cue
(define (fade-preset-val this-cue-state next-cue-state fix attr)
  (if next-cue-state
      (let ((next-cue-val (state-find fix attr next-cue-state))
            (this-cue-intensity (state-find fix 'intensity this-cue-state)))
        (if (dark? this-cue-intensity)
            next-cue-val
            'no-value))
      'no-value))


;; Work out how many seconds 'fix' will take to complete its intensity fade
;; NB Don't worry about whether it makes sense to do a preset or not
;; - that's already taken care of in fade-preset-val
(define (calc-preset-start-time fix the-cue)
  (let ((fade-times (cue-part-fade-times the-cue fix 'intensity)))
    (+ (get-fade-down-time fade-times)
       (get-fade-down-delay fade-times)
       (get-fade-preset-delay fade-times))))


(define (fix-attr-eq fa1 fa2)
  (and (eq? (car fa1) (car fa2))
       (eq? (cdr fa1) (cdr fa2))))


(define (fix-attrs-in-state state)
  (state-map (lambda (fix attr val) (cons fix attr))
             state))


(define (add-fix-attrs-to-list state old-list)
  (lset-union fix-attr-eq
              old-list
              (fix-attrs-in-state state)))


(define (fix-attrs-involved . states)
  (fold add-fix-attrs-to-list '() states))


(define (make-fade-for-attribute-type type)
  (cond
    ((eq? type 'continuous) make-continuous-attr-fade)
    ((eq? type 'list) make-list-attr-fade)
    (else
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Unrecognised attribute type")
                         (make-exception-with-irritants type))))))


(define (run-cue-index! pb cue-index)
  (let ((this-cue-state (calculate-tracking (get-playback-cue-list pb) cue-index))
        (next-cue-state (calculate-tracking (get-playback-cue-list pb) (+ cue-index 1)))
        (the-cue (vector-ref (get-playback-cue-list pb) cue-index))
        (tnow (hirestime)))

    (for-each
      (lambda (fix-attr)

        (let* ((fix (car fix-attr))
               (attr (cdr fix-attr))
               (fade-times (cue-part-fade-times the-cue fix attr))
               (start-val (fade-start-val tnow pb fix attr))
               (target-val (state-find fix attr this-cue-state))
               (preset-val (fade-preset-val this-cue-state next-cue-state fix attr)))

          (if (intensity? attr)

              ;; Intensity attribute
              (set-in-state! pb fix attr
                             (make-intensity-fade start-val
                                                  target-val
                                                  fade-times
                                                  tnow))

              ;; Non-intensity attribute
              (let ((attribute-obj (find-attr fix attr)))

                (unless attribute-obj
                  (raise-exception (make-exception
                                     (make-exception-with-message
                                       "Attribute not found")
                                     (make-exception-with-irritants
                                       (list fix attr)))))

                (let* ((atype (get-attr-type attribute-obj))
                       (make-fade-func (make-fade-for-attribute-type atype))
                       (fade-start-time (+ tnow (get-fade-attr-delay fade-times)))
                       (preset-start-time (+ tnow (calc-preset-start-time fix the-cue))))

                  (set-in-state! pb fix attr
                                 (make-fade-func start-val
                                                 target-val
                                                 preset-val
                                                 (get-fade-attr-time fade-times)
                                                 fade-start-time
                                                 (get-fade-preset-time fade-times)
                                                 preset-start-time)))))))

      ;; Add the next cue to list of states to look at, only if it exists)
      (if next-cue-state
          (fix-attrs-involved pb this-cue-state next-cue-state)
          (fix-attrs-involved pb this-cue-state)))))


(define (print-playback pb)
  (format #t "Playback ~a:\n" pb)
  ;;(format #t "        Cue list ~a\n" (get-playback-cue-list pb))
  (if (< (get-next-cue-index pb)
         (vector-length (get-playback-cue-list pb)))
      (let ((the-cue (vector-ref (get-playback-cue-list pb)
                                 (get-next-cue-index pb))))
        (format #t "  Next cue index ~a (~a)\n"
                (get-next-cue-index pb)
                the-cue))
      (format #t "  End of cue list.\n"))
  *unspecified*)


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


(define (ensure-cue-zero-realized the-cue-list)
  (let ((cue-zero (vector-ref the-cue-list 0)))
    (unless (get-tracked-state cue-zero)
      (parameterize ((current-state (make-empty-state)))
        (set-tracked-state! cue-zero (current-state))))))


;; Get the state for a cue, taking into account tracking etc
(define (calculate-tracking the-cue-list cue-index)

  (ensure-cue-zero-realized the-cue-list)

  (if (>= cue-index (vector-length the-cue-list))
      #f
      (let* ((the-cue (vector-ref the-cue-list cue-index))
             (rstate (get-tracked-state the-cue)))
        (or rstate
            (let ((previous-state (calculate-tracking the-cue-list (- cue-index 1))))
              (parameterize ((current-state (make-empty-state)))
                (apply-state previous-state)
                (unless (track-intensities the-cue)
                  (blackout (current-state)))
                (apply-state (get-cue-state the-cue))
                (set-tracked-state! the-cue (current-state))
                (current-state)))))))


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
