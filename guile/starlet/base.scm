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
             make-workspace scanout-freq
             make-playback cue cut-to-cue
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


(define-generic set-in-state!)

(define-method (set-in-state! (state <starlet-state>)
                              (fix <fixture>)
                              (attr <fixture-attribute>)
                              value)
  (hash-set! (get-state-hash-table state)
             (cons fix attr)
             value))


;; A "workspace" is just a "state" with extra information
;; about how its contents should be sent out on the wire
(define-class <starlet-workspace> (<starlet-state>)
  (priority
   #:init-value 0
   #:init-keyword #:priority
   #:getter get-workspace-priority
   #:setter set-workspace-priority!))


(define-record-type <fade>
  (make-fade state target-frac fade-time fade-delay start-time)
  fade?
  (state        get-fade-state)
  (target-frac  get-fade-target-frac)
  (fade-time    get-fade-time)
  (fade-delay   get-fade-delay)
  (start-time   get-fade-start-time))


(define-record-type <cue>
  (make-cue number state-func up-time down-time up-delay down-delay)
  cue?
  (number      get-cue-number)
  (state-func  get-cue-state-func)
  (up-time     up-time)
  (up-delay    up-delay)
  (down-time   down-time)
  (down-delay  down-delay))


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


(define-class <starlet-playback> (<starlet-workspace>)
  (active-state
   #:init-value '()
   #:getter get-active-state-list
   #:setter set-active-state-list!)
  (cue-list
   #:init-keyword #:cue-list
   #:getter get-playback-cue-list))


(define (make-playback cue-list)
  (let ((new-playback (make <starlet-playback>
                        #:cue-list cue-list)))
    (add-to-workspace-list new-playback)
    new-playback))


(define (find-cue cue-list cue-number)
  (find (lambda (a)
          (eqv? (get-cue-number a)
                cue-number))
        cue-list))


(define (cut-to-cue pb cue-number)
  (let* ((cue-state-func
          (get-cue-state-func
           (find-cue (get-playback-cue-list pb)
                     cue-number))))

    ;; Flush everything out and just set the state
    (set-active-state-list! pb
                            (list (make-fade
                                   (cue-state-func pb)
                                   1.0 0.0 0.0 (hirestime))))))


;; List of fixtures
(define patched-fixture-list (make-atomic-box '()))

;; Basic workspace which holds everything at "home" unless
;; commanded otherwise
(define base-workspace (make <starlet-workspace>
                         #:priority -100))

;; List of workspaces
(define workspace-list (make-atomic-box (list base-workspace)))


;; Set a single attribute to home position
(define (home-attr! state fix attr)
  (set-in-state! state
                 fix
                 attr
                 (get-attr-home-value attr)))


;; Set all attributes of a fixture to home position
(define (home-all! workspace fix)
  (for-each (lambda (attr)
              (home-attr! workspace fix attr))
            (slot-ref fix 'attributes)))


;; Set the intensity of all patched fixtures to zero
(define (blackout workspace)
  (for-each (lambda (fix)
              (set-attr! workspace fix 'intensity 0))
            (atomic-box-ref patched-fixture-list)))


(define (find-attr fix attr-name)
  (find (lambda (a)
          (eq? (get-attr-name a)
               attr-name))
        (slot-ref fix 'attributes)))

(define (add-to-workspace-list new-workspace)
  (atomic-box-set! workspace-list
                   (cons new-workspace
                         (atomic-box-ref workspace-list))))

(define (make-workspace)
  (let ((new-workspace (make <starlet-workspace>)))
    (add-to-workspace-list new-workspace)
    new-workspace))


;; Set an attribute
(define (set-attr! workspace fix attr-name value)
  (let ((attr (find-attr fix attr-name)))
    (when attr (set-in-state! workspace fix attr value))))


(define (fade-frac fade-time start-time time-now)
  (min (/ (- time-now start-time)
          fade-time)
       1.0))


(define (wrap-fade value fade-time start-time)
  (lambda (time)
    (inexact->exact (* (value->number value time)
                       (fade-frac fade-time
                                  start-time
                                  (hirestime))))))


;; "state" is a function with one parameter: a workspace
;; This function sets up "workspace" to fade in the state
(define* (fade-up workspace state
                  #:key (fade-time 5))
  (let ((fade-up-state (make <starlet-state>))
        (start-time (hirestime)))

    ;; Execute passed-in function to get state
    (state fade-up-state)

    (state-for-each (lambda (fix attr value)
                      (set-in-state! fade-up-state
                                     fix
                                     attr
                                     (wrap-fade value fade-time start-time)))
                    fade-up-state)

    (set-state-hash-table! workspace
                           (get-state-hash-table fade-up-state))))


;; Patch a new fixture
(define* (patch-fixture class
                        start-addr
                        #:key (universe 1) (friendly-name "Fixture"))
  (let ((new-fixture (make class
                       #:sa start-addr
                       #:uni universe
                       #:friendly-name friendly-name)))
    (home-all! base-workspace new-fixture)
    (atomic-box-set! patched-fixture-list
                     (cons new-fixture
                           (atomic-box-ref patched-fixture-list)))
    new-fixture))



;; Helper functions for attribute translators
(define (round-dmx a)
  (min 255 (max 0 (round a))))

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
                                         (wrap-merge merge-rule current-value value))
                          (set-in-state! combined-state
                                         fix
                                         attr
                                         value))))
                 new))


(define (value->number val time)
  (if (procedure? val)
      (val time)
      val))


(define (wrap-merge merge-rule a b)
  (lambda (time)
    (merge-rule (value->number a time)
                (value->number b time))))


(define (merge-rule-ltp a b)
  b)

(define (merge-rule-htp a b)
  (max a b))


;; Combine states
(define (merge-states merge-rule list-of-workspaces)
  (let ((combined-state (make <starlet-state>)))
    (for-each (lambda (workspace)
                (add-state-to-state merge-rule
                                    workspace
                                    combined-state))
              list-of-workspaces)
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

          ;; Scan out all attributes of the combined workspace
          (state-for-each (lambda (fix attr value)

                            ;; Scan out one attribute assignment
                            (let ((trans (get-attr-translator attr)))
                              (trans (get-fixture-universe fix)
                                     (get-fixture-addr fix)
                                     (value->number value (hirestime))
                                     set-dmx)))

                          (merge-states merge-rule-htp
                                        (atomic-box-ref
                                         workspace-list)))

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
