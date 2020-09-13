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
            cue-state
            qnum))


;; A "playback" is a state which knows how to run cues
;; from a cue list
(define-class <starlet-playback> (<starlet-state>)
  (active-fade-list
   #:init-value '()
   #:getter get-active-fade-list
   #:setter set-active-fade-list!)

  (cue-list
   #:init-keyword #:cue-list
   #:getter get-playback-cue-list)

  (next-cue-index
   #:init-value 0
   #:getter get-next-cue-index
   #:setter set-next-cue-index!)

  (hash-table
   #:allocation #:virtual
   #:getter get-state-hash-table
   #:slot-ref (lambda (instance)
                (merge-active-fades (hirestime)
                                    (get-active-fade-list instance)))
   #:slot-set! (lambda (instance new-val)
                 (error "Can't set hash table on playback"))))


(define-record-type <fade>
  (make-fade state start-frac target-frac fade-time fade-delay start-time)
  fade?
  (start-frac   get-fade-start-frac)
  (target-frac  get-fade-target-frac)
  (fade-time    get-fade-time)
  (state        get-fade-state)
  (start-time   get-fade-start-time)
  (fade-delay   get-fade-delay-time))


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


(define (wrap-scale scale-factor a)
  (lambda (time)
    (* (value->number a time)
       scale-factor)))


(define (get-current-fraction fade current-time)
  (let ((elapsed-fade-time (- current-time
                              (get-fade-start-time fade)
                              (get-fade-delay-time fade))))
    (cond

     ;; Before start of fade
     ((< elapsed-fade-time 0)
      (get-fade-start-frac fade))

     ;; After end of fade
     ((> elapsed-fade-time (get-fade-time fade))
      (get-fade-target-frac fade))

     ;; During the fade
     (else
      (+ (get-fade-start-frac fade)
         (* (- (get-fade-target-frac fade)
               (get-fade-start-frac fade))

            ;; Fraction of fade time elapsed
            (/ elapsed-fade-time
               (get-fade-time fade))))))))


(define (scale-fade fade current-time)
  (let ((state (make-empty-state))
        (scale-factor (get-current-fraction fade current-time)))
    (state-for-each (lambda (fix attr value)
                      (if (intensity? attr)
                          (set-in-state! state
                                         fix
                                         attr
                                         (wrap-scale scale-factor value))
                          (set-in-state! state fix attr value)))
                    (get-fade-state fade))
    state))


(define (merge-active-fades current-time list-of-fades)
  (get-state-hash-table
   (merge-states-htp
    (map (lambda (fade) (scale-fade fade current-time))
         list-of-fades))))


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
         (cue-index (cue-number-to-index cue-list cue-number)))
    (set-active-fade-list! pb
                           (list (make-fade
                                  (realize-state cue-list cue-index)
                                  0.0 1.0 0.0 0.0 (hirestime))))
    (set-next-cue-index! pb (+ cue-index 1)))
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
      (let ((the-cue (vector-ref cue-list cue-index))
            (tnow (hirestime)))
        (retire-old-fades! pb tnow)
        (fade-down-all-active-states! pb
                                      tnow
                                      (down-time the-cue)
                                      (down-delay the-cue))
        (add-fade! pb (make-fade-from-cue cue-list cue-index tnow)))
      (set-next-cue-index! pb (+ cue-index 1))))
      ;; else at the end of the cue list
  (return-unspecified))


(define (add-fade! pb fade)
  (set-active-fade-list! pb
                         (cons fade
                               (get-active-fade-list pb))))


(define (make-fade-from-cue cue-list cue-index time)
  (let ((the-cue (vector-ref cue-list cue-index)))
    (make-fade
     (realize-state cue-list cue-index)
     0.0
     1.0
     (up-time the-cue)
     (up-delay the-cue)
     time)))


(define (retire-old-fades! pb tnow)
  (set-active-fade-list!
   pb
   (filter (lambda (a)
             (or
              (< tnow
                 (+ (get-fade-start-time a)
                    (get-fade-delay-time a)
                    (get-fade-time a)))
              (> (get-fade-target-frac a)
                 0.0)))
           (get-active-fade-list pb))))


(define (fade-down-all-active-states! pb tnow down-time down-delay)
  (set-active-fade-list!
   pb
   (map (lambda (a)
          (make-fade
           (get-fade-state a)
           (get-current-fraction a tnow)
           0.0
           down-time
           down-delay
           tnow))
        (get-active-fade-list pb))))


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
                         home-state)))


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
              (apply-state blackout-state))
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
