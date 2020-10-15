(define-module (starlet playback)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (starlet base)
  #:use-module (starlet utils)
  #:export (make-playback
            cue
            cut-to-cue-number!
            run-cue-number!
            go!
            cue-list
            cue-state))


;; A "playback" is a state which knows how to run cues
;; from a cue list
(define-class <starlet-playback> (<starlet-state>)
  (cue-list
   #:init-keyword #:cue-list
   #:getter get-playback-cue-list)

  (next-cue-index
   #:init-value 0
   #:getter get-next-cue-index
   #:setter set-next-cue-index!)

  (fade-records
   #:init-form (make-hash-table)
   #:getter get-fade-records
   #:setter set-fade-records!))


(define-record-type <fade-record>
  (make-fade-record start-time
                    up-time
                    down-time
                    up-delay
                    down-delay
                    previous
                    target)
  fade-record?
  (start-time         fade-start-time)
  (up-time            fade-up-time)
  (down-time          fade-down-time)
  (up-delay           fade-up-delay)
  (down-delay         fade-down-delay)
  (previous           fade-previous)
  (target             fade-target))


(define-record-type <cue>
  (make-cue number
            state-function
            realized-state
            up-time
            down-time
            up-delay
            down-delay
            track-intensities)
  cue?
  (number             get-cue-number)
  (state-function     get-cue-state-function)
  (realized-state     get-realized-state set-realized-state!)
  (up-time            up-time)
  (up-delay           up-delay)
  (down-time          down-time)
  (down-delay         down-delay)
  (track-intensities  track-intensities))


(define (qnum a)
  (/ (inexact->exact (* a 1000)) 1000))


(define (make-playback cue-list)
  (let ((new-playback (make <starlet-playback>
                        #:cue-list cue-list)))
    new-playback))


(define (cue-number-to-index cue-list cue-number)
  (vector-index (lambda (a)
                  (eqv? (get-cue-number a)
                        cue-number))
                cue-list))


(define (cut-to-cue-number! pb cue-number)
  (let* ((cue-list (get-playback-cue-list pb))
         (cue-index (cue-number-to-index cue-list (qnum cue-number))))
    (set-state-hash-table! pb (get-state-hash-table
                               (realize-state cue-list
                                              cue-index)))
    (set-next-cue-index! pb (+ cue-index 1))

    ;; Wipe out the old fade params
    (set-fade-records! pb (make-hash-table))

    ;; Record fade params
    (state-for-each
     (lambda (fix attr val)
             (hash-set! (get-fade-records pb)
                        (cons fix attr)
                        (make-fade-record (hirestime)
                                          0.0
                                          0.0
                                          0.0
                                          0.0
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
                 up-time
                 down-time
                 up-delay
                 down-delay
                 tnow)
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
                time))))


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

  (let ((prev-val (fade-previous fade-record))
        (target (fade-target fade-record)))

    (cond

     ;; Number to number, fading up
     ((and (number? target) (number? prev-val) (> target prev-val))
      (set-in-state! pb fix attr (wrap-fade prev-val
                                            target
                                            (fade-up-time fade-record)
                                            (fade-up-delay fade-record)
                                            (fade-start-time fade-record))))

     ;; Number to number, fading down
     ((and (number? target) (number? prev-val) (< target prev-val))
      (set-in-state! pb fix attr (wrap-fade prev-val
                                            target
                                            (fade-down-time fade-record)
                                            (fade-down-delay fade-record)
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
                                          (fade-up-time fade-record)
                                          (fade-down-time fade-record)
                                          (fade-up-delay fade-record)
                                          (fade-down-delay fade-record)
                                          (fade-start-time fade-record)))))))


(define (fade-finished? tnow fade-record)
  (and
   (> tnow
      (+ (fade-start-time fade-record)
         (fade-up-delay fade-record)
         (fade-up-time fade-record)))
   (> tnow
      (+ (fade-start-time fade-record)
         (fade-down-delay fade-record)
         (fade-down-time fade-record)))))


(define (run-cue-index! pb cue-list cue-number tnow)

  (let ((the-cue-state (realize-state cue-list cue-number))
        (the-cue (vector-ref cue-list cue-number)))

    (state-for-each
     (lambda (fix attr val)

       (let ((fade-record (hash-ref (get-fade-records pb)
                                    (cons fix attr))))
         (let ((new-record (make-fade-record tnow
                                             (up-time the-cue)
                                             (down-time the-cue)
                                             (up-delay the-cue)
                                             (down-delay the-cue)
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


(define* (cue number
              state-function
              #:key
              (fade-up 5)
              (fade-down 5)
              (up-delay 0)
              (down-delay 0)
              (track-intensities #f))
  (make-cue (qnum number)
            state-function
            #f
            fade-up
            fade-down
            up-delay
            down-delay
            track-intensities))

(define (ensure-cue-zero-realized cue-list)
  (unless (get-realized-state (vector-ref cue-list 0))
    (set-realized-state! (vector-ref cue-list 0)
                         (make <starlet-state>))))


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
                  #:fade-up 0
                  #:fade-down 0)
             body ...))))
