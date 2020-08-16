(define-module (starlet base)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (srfi srfi-9)
  #:export (<fixture> <fixture-attribute>
             start-ola-output patch-fixture
             set-attr! home-attr! home-all! blackout
             scanout-freq make-empty-state register-state!
             make-playback cue cut-to-cue! run-cue!
             percent->dmxval msb lsb chan))

(use-modules (srfi srfi-1))


(define-class <fixture-attribute> (<object>)
  (name
    #:init-value 'unnamed-attribute
    #:init-keyword #:name
    #:getter get-attr-name)

  (range
    #:init-value '()
    #:init-keyword #:range
    #:getter get-attr-range)

  (translator
    #:init-value (lambda (universe start-addr value set-dmx) #f)
    #:init-keyword #:translator
    #:getter get-attr-translator)

  (type
    #:init-value 'continuous
    #:init-keyword #:type
    #:getter get-attr-type)

  (home-value
    #:init-value 0
    #:init-keyword #:home-value
    #:getter get-attr-home-value
    #:setter set-attr-home-value!))


(define-class <fixture> (<object>)
  (universe
    #:init-value #f
    #:init-keyword #:uni
    #:getter get-fixture-universe
    #:setter set-fixture-universe!)

  (start-addr
    #:init-value #f
    #:init-keyword #:sa
    #:getter get-fixture-addr
    #:setter set-fixture-addr!)

  (friendly-name
    #:init-value "Fixture"
    #:init-keyword #:friendly-name
    #:getter get-fixture-friendly-name
    #:setter set-fixture-friendly-name!))


;; A "state" is just a thin wrapper around a hash table
;; of (fixture . attribute)  -->  value
(define-class <starlet-state> (<object>)
  (hash-table
   #:init-form (make-hash-table)
   #:getter get-state-hash-table
   #:setter set-state-hash-table!))


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


(define-method (set-in-state! (state <starlet-state>)
                              (fix <fixture>)
                              (attr <fixture-attribute>)
                              value)
  (hash-set! (get-state-hash-table state)
             (cons fix attr)
             value))


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


;; List of fixtures
(define patched-fixture-list (make-atomic-box '()))

;; Basic state which holds everything at "home" unless
;; commanded otherwise
(define home-state (make <starlet-state>))

(define (make-empty-state)
  (make <starlet-state>))

;; List of states being scanned out
(define state-list (make-atomic-box (list home-state)))


;; Set a single attribute to home position
(define (home-attr! state fix attr)
  (set-in-state! state
                 fix
                 attr
                 (get-attr-home-value attr)))


;; Set all attributes of a fixture to home position
(define (home-all! state fix)
  (for-each (lambda (attr)
              (home-attr! state fix attr))
            (slot-ref fix 'attributes)))


;; Set the intensity of all patched fixtures to zero
(define (blackout state)
  (for-each (lambda (fix)
              (set-attr! state fix 'intensity 0))
            (atomic-box-ref patched-fixture-list)))


(define (find-attr fix attr-name)
  (find (lambda (a)
          (eq? (get-attr-name a)
               attr-name))
        (slot-ref fix 'attributes)))

(define (register-state! new-state)
  (atomic-box-set! state-list
                   (cons new-state
                         (atomic-box-ref state-list))))

;; Set an attribute by name
(define (set-attr! state fix attr-name value)
  (let ((attr (find-attr fix attr-name)))
    (when attr (set-in-state! state fix attr value))))


(define (fade-frac fade-time start-time time-now)
  (min (/ (- time-now start-time)
          fade-time)
       1.0))


;; Patch a new fixture
(define* (patch-fixture class
                        start-addr
                        #:key (universe 1) (friendly-name "Fixture"))
  (let ((new-fixture (make class
                       #:sa start-addr
                       #:uni universe
                       #:friendly-name friendly-name)))
    (home-all! home-state new-fixture)
    (atomic-box-set! patched-fixture-list
                     (cons new-fixture
                           (atomic-box-ref patched-fixture-list)))
    new-fixture))



;; Helper functions for attribute translators
(define (round-dmx a)
  (inexact->exact
   (min 255 (max 0 (round a)))))

(define (percent->dmxval val)
  (round-dmx (/ (* 256 val) 100)))

(define (msb val)
  (round-dmx (/ val 256)))

(define (lsb val)
  (round-dmx (logand (round val) #b11111111)))

(define (chan channel start-addr)
  (- (+ channel start-addr) 1))


(define (state-for-each func state)
  (hash-for-each (lambda (key value)
                   (func (car key)
                         (cdr key)
                         value))
                 (get-state-hash-table state)))

(define (state-find fix attr state)
  (hash-ref (get-state-hash-table state)
            (cons fix attr)))


;; Add the contents of state "new" to "combined-state"
(define (add-state-to-state merge-rule new combined-state)
  (state-for-each (lambda (fix attr value)
                    (let ((current-value (state-find fix
                                                     attr
                                                     combined-state)))
                      (if current-value
                          (set-in-state! combined-state
                                         fix
                                         attr
                                         (merge-rule attr current-value value))
                          (set-in-state! combined-state
                                         fix
                                         attr
                                         value))))
                 new))


(define (value->number val time)
  (if (procedure? val)
      (val time)
      val))


;; If "state" is a procedure, call it to get the real state
;; Otherwise, pass through
(define (expand-state state)
  (if (procedure? state)
      (state)
      state))


(define (merge-rule-ltp attr a b)
  (lambda (time)
    (value->number b time)))

(define (merge-rule-htp attr a b)
  (if (eq? 'intensity (get-attr-name attr))

      ;; HTP only for intensity attributes
      (lambda (time)
        (max (value->number a time)
             (value->number b time)))

      ;; LTP for all non-intensity attributes
      (lambda (time)
        (value->number b time))))

(define (merge-states-htp list-of-states)
  (merge-states merge-rule-htp
                list-of-states))

;; Combine states
(define (merge-states merge-rule list-of-states)
  (let ((combined-state (make <starlet-state>)))
    (for-each (lambda (state)
                (add-state-to-state merge-rule
                                    (expand-state state)
                                    combined-state))
              list-of-states)
    combined-state))


;; Scanout
(define (bytevec->string bv)
  (string-join
    (map
      number->string
      (u8vector->list bv))
    ","))


(define (send-to-ola ola-uri ola-socket universe)
  (http-post
    ola-uri
    #:port ola-socket
    #:keep-alive? #t
    #:headers (acons 'content-type
                     (parse-header 'content-type
                                   "application/x-www-form-urlencoded")
                     '())
    #:body (string-append "u="
                          (number->string (car universe))
                          "&d="
                          (bytevec->string (cdr universe)))))


(define (hirestime)
  (let ((a (gettimeofday)))
    (+ (car a)
       (/ (cdr a)
          1000000))))

(define scanout-freq 0)

(define (start-ola-output)
  (let* ((ola-uri (build-uri 'http
                             #:host "127.0.0.1"
                             #:port 9090
                             #:path "/set_dmx"))
         (ola-socket (open-socket-for-uri ola-uri))
         (start-time (hirestime)))

    (begin-thread
     (let scanout-loop ((count 0))

        (let ((universes '()))


          ;; Helper function called by attribute translators
          ;; to set individual DMX values
          (define (set-dmx universe addr value)

            ;; Create DMX array for universe if it doesn't exist already
            (unless (assq universe universes)
              (set! universes (acons universe
                                     (make-u8vector 512 0)
                                     universes)))

            ;; Set the value in the DMX array
            (u8vector-set! (assq-ref universes universe)
                           (- addr 1)                   ; u8vector-set indexing starts from zero
                           (round-dmx value)))

          ;; Scan out all attributes of the combined state
          (state-for-each (lambda (fix attr value)

                            ;; Scan out one attribute assignment
                            (let ((trans (get-attr-translator attr)))
                              (trans (get-fixture-universe fix)
                                     (get-fixture-addr fix)

                                     ;; This function call triggers evaluation of
                                     ;; the whole chain of attribute functions,
                                     ;; right down to a real number
                                     (value->number value (hirestime))

                                     ;; Pass a helper function to set DMX values
                                     set-dmx)))

                          (merge-states-htp

                           ;; Reverse in order to put "home" state last
                           (reverse
                            (atomic-box-ref state-list))))

          ;; Send everything to OLA
          (for-each (lambda (a)
                      (send-to-ola ola-uri ola-socket a))
                    universes))

        (usleep 10000)
        (if (eq? count 100)
            (begin
              (set! scanout-freq
                (exact->inexact (/ 100
                                   (- (hirestime) start-time))))
              (set! start-time (hirestime))
              (scanout-loop 0))
            (scanout-loop (+ count 1)))))))
