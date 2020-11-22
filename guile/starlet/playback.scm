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
            cue-state
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
                   up-delay
                   down-delay)
  fade-times?
  (up-time      get-fade-up-time)
  (down-time    get-fade-down-time)
  (up-delay     get-fade-up-delay)
  (down-delay   get-fade-down-delay))


;; Macro to avoid a profusion of (get-fade-xxx-time fade-times)
(define-syntax with-fade-times
  (lambda (x)
    (syntax-case x ()
      ((_ fade-times body ...)
       (with-syntax ((up-time (datum->syntax x 'up-time))
                     (down-time (datum->syntax x 'down-time))
                     (up-delay (datum->syntax x 'up-delay))
                     (down-delay (datum->syntax x 'down-delay)))
         #'(let ((up-time (get-fade-up-time fade-times))
                 (down-time (get-fade-down-time fade-times))
                 (up-delay (get-fade-up-delay fade-times))
                 (down-delay (get-fade-down-delay fade-times)))
             body ...))))))


(define-record-type <fade-record>
  (make-fade-record start-time
                    fade-times
                    previous
                    target)
  fade-record?
  (start-time         fade-start-time)
  (fade-times         get-fade-record-fade-times)
  (previous           fade-previous)
  (target             fade-target))


(define-record-type <cue>
  (make-cue number
            state-function
            realized-state
            fade-times
            track-intensities
            cue-parts)
  cue?
  (number             get-cue-number)
  (state-function     get-cue-state-function)
  (realized-state     get-realized-state set-realized-state!)
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
                                (realize-state cue-list cue-index))))
    (set-next-cue-index! pb (+ cue-index 1))

    ;; Wipe out the old fade params
    (set-fade-records! pb (make-hash-table))

    ;; Record fade params
    (state-for-each
     (lambda (fix attr val)
             (hash-set! (get-fade-records pb)
                        (cons fix attr)
                        (make-fade-record (hirestime)
                                          (make-fade-times
                                           0.0
                                           0.0
                                           0.0
                                           0.0)
                                          0.0
                                          val)))
     pb))

  (return-unspecified))


(define (go! pb)
  (let ((cue-index (get-next-cue-index pb)))
    (run-cue! pb cue-index))
  (return-unspecified))


(define (run-cue-number! pb cue-number)
  (let ((cue-index (cue-number-to-index (get-playback-cue-list pb)
                                        cue-number)))
    (run-cue! pb cue-index))
  (return-unspecified))


(define (run-cue! pb cue-index)
  (let* ((cue-list (get-playback-cue-list pb)))
    (unless (>= cue-index (vector-length cue-list))
      (run-cue-index! pb cue-list cue-index (hirestime))
      (set-next-cue-index! pb (+ cue-index 1))))
      ;; else at the end of the cue list
  (return-unspecified))


(define (fade-func start-val end-val fade-time delay-time start-time current-time)
  (let ((elapsed-fade-time (- current-time start-time delay-time)))
    (cond

     ;; Before start of fade
     ((< elapsed-fade-time 0)
      start-val)

     ;; After end of fade
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
                   fade-time
                   delay-time
                   tnow)
  (lambda (time)
    (fade-func (value->number start-val time)
               (value->number end-val time)
               fade-time
               delay-time
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
                 down-time
                 down-delay
                 tnow
                 time)
      (fade-func 0
                 (value->number end-val time)
                 up-time
                 up-delay
                 tnow
                 time)))))


(define (fade-start-val tnow pb old-fade-record fix attr val)
  (cond

   ;; Attr not seen before in this playback: start fading from home
   ((eq? old-fade-record #f)
    (home-val fix attr))

   ;; Attr seen in a finished fade
   ((fade-finished? tnow old-fade-record)
    (fade-target old-fade-record))

   ;; Attr is currently fading: get the current state
   ;; (NB it might be a function/effect)
   (else
    (let ((func (state-find fix attr pb)))
      (func tnow)))))


(define (set-fade pb fix attr fade-record)

  (with-fade-times
   (get-fade-record-fade-times fade-record)
   (let ((prev-val (fade-previous fade-record))
         (target (fade-target fade-record)))

     (cond

      ;; Number to number, fading up
      ((and (number? target) (number? prev-val) (> target prev-val))
       (set-in-state! pb fix attr (wrap-fade prev-val
                                             target
                                             up-time
                                             up-delay
                                             (fade-start-time fade-record))))

      ;; Number to number, fading down
      ((and (number? target) (number? prev-val) (< target prev-val))
       (set-in-state! pb fix attr (wrap-fade prev-val
                                             target
                                             down-time
                                             down-delay
                                             (fade-start-time fade-record))))

      ;; Number to number, staying the same
      ((and (number? target) (number? prev-val))
       (set-in-state! pb fix attr (wrap-fade prev-val
                                             target
                                             0.0
                                             0.0
                                             (fade-start-time fade-record))))

      ;; Everything else, e.g. number to effect
      (else
       (set-in-state! pb fix attr (wrap-xf (fade-previous fade-record)
                                           (fade-target fade-record)
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
          down-time)))))


(define (match-fix-attr attr-el fix attr)
  (cond

   ((fixture? attr-el)
    (eq? attr-el fix))

   ((and (pair? attr-el)
         (fixture? (car attr-el))
         (fixture-attribute? (cdr attr-el)))
    (and (eq? (car attr-el) fix)
         (eq? (cdr attr-el) attr)))

   ((and (pair? attr-el)
         (fixture? (car attr-el))
         (symbol? (cdr attr-el)))
    (and (eq? (car attr-el) fix)
         (eq? (cdr attr-el) (get-attr-name attr))))

   ((list? attr-el)
    (and (memq fix attr-el)
         (or (memq attr attr-el)
             (memq (get-attr-name attr) attr-el))))

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


(define (run-cue-index! pb cue-list cue-number tnow)

  (let ((the-cue-state (realize-state cue-list cue-number))
        (the-cue (vector-ref cue-list cue-number)))

    (state-for-each
     (lambda (fix attr val)

       (let ((fade-record (hash-ref (get-fade-records pb)
                                    (cons fix attr))))
         (let ((new-record (make-fade-record tnow
                                             (cue-part-fade-times the-cue fix attr)
                                             (fade-start-val tnow
                                                             pb
                                                             fade-record
                                                             fix
                                                             attr
                                                             val)
                                             val)))
           (hash-set! (get-fade-records pb)
                      (cons fix attr)
                      new-record)
           (set-fade pb fix attr new-record))))

     the-cue-state)))


;;; ******************** Cue lists ********************

(define-syntax cue-state
  (syntax-rules ()
    ((_ body ...)
     (lambda ()
       body ...))))


(define-syntax cue-part
  (syntax-rules ()
    ((_ (fixtures ...) params ...)
     (make-cue-part-obj (list fixtures ...)
                        params ...))))


(define* (make-cue-part-obj attr-list
                            #:key
                            (up-time 5)
                            (down-time 5)
                            (up-delay 0)
                            (down-delay 0))
  (make-cue-part attr-list
                 (make-fade-times
                  up-time
                  down-time
                  up-delay
                  down-delay)))


(define cue
  (lambda (number state-function . rest)
    (receive (cue-parts rest-minus-cue-parts)
        (partition cue-part? rest)
      (let-keywords rest-minus-cue-parts #f
                    ((up-time 5)
                     (down-time 5)
                     (up-delay 0)
                     (down-delay 0)
                     (track-intensities #f))

                    (make-cue (qnum number)
                              state-function
                              #f
                              (make-fade-times
                               up-time
                               down-time
                               up-delay
                               down-delay)
                              track-intensities
                              cue-parts)))))


(define (ensure-cue-zero-realized cue-list)
  (let ((cue-zero (vector-ref cue-list 0)))
    (unless (get-realized-state cue-zero)
      (set-realized-state! cue-zero (make <starlet-state>)))))


;; Get the state for a cue, taking into account tracking etc
(define (realize-state cue-list cue-index)

  (ensure-cue-zero-realized cue-list)

  (let* ((the-cue (vector-ref cue-list cue-index))
         (rstate (get-realized-state the-cue)))
    (or rstate
        (let ((previous-state (realize-state cue-list (- cue-index 1))))
          (parameterize ((current-state (make-empty-state)))
            (apply-state previous-state)
            (unless (track-intensities the-cue)
              (blackout (current-state)))
            ((get-cue-state-function the-cue))
            (set-realized-state! the-cue (current-state))
            (current-state))))))


(define-syntax cue-list
  (syntax-rules ()
    ((_ body ...)
     (vector (cue 0
                  (lambda () #f)   ;; The real base state is in ensure-cue-zero-realized
                  #:up-time 0
                  #:down-time 0)
             body ...))))
