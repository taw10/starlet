(define-module (starlet playback)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (starlet base)
  #:export (make-playback cue cut-to-cue! run-cue!))


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
  (make-cue number state up-time down-time up-delay down-delay)
  cue?
  (number      get-cue-number)
  (state       get-cue-state)
  (up-time     up-time)
  (up-delay    up-delay)
  (down-time   down-time)
  (down-delay  down-delay))


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
     ((> elapsed-fade-time
         (get-fade-time fade))
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
                      (if (eq? 'intensity (get-attr-name attr))
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


(define* (cue number
              state
              #:key (fade-up 5) (fade-down 5) (up-delay 0) (down-delay 0))
  (make-cue (qnum number)
            state
            fade-up
            fade-down
            up-delay
            down-delay))


(define (make-playback cue-list)
  (let ((new-playback (make <starlet-playback>
                        #:cue-list cue-list)))
    new-playback))


(define (find-cue cue-list cue-number)
  (find (lambda (a)
          (eqv? (get-cue-number a)
                cue-number))
        cue-list))


(define (cut-to-cue! pb cue-number)
  (let* ((state (expand-state
                 (get-cue-state
                  (find-cue (get-playback-cue-list pb)
                            cue-number)))))

    ;; Flush everything out and just set the state
    (set-active-fade-list! pb
                           (list (make-fade
                                  state
                                  0.0 1.0 0.0 0.0 (hirestime))))))


(define (add-fade! pb fade)
  (set-active-fade-list! pb
                         (cons fade
                               (get-active-fade-list pb))))


(define (make-fade-from-cue cue time)
  (make-fade
   (expand-state (get-cue-state cue))
   0.0
   1.0
   (up-time cue)
   (up-delay cue)
   time))


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


(define (run-cue! pb cue-number)
  (let ((tnow (hirestime))
        (cue (find-cue (get-playback-cue-list pb)
                       cue-number)))
    (retire-old-fades! pb tnow)
    (fade-down-all-active-states! pb
                                  tnow
                                  (down-time cue)
                                  (down-delay cue))
    (add-fade! pb (make-fade-from-cue cue tnow))))
