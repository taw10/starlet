(define-module (starlet base)
  #:use-module (starlet utils)
  #:use-module (starlet colours)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 exceptions)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<fixture>
            patch-fixture!
            get-attributes
            get-fixture-name
            find-attr
            fixture?
            scanout-fixture

            <fixture-attribute>
            attr-continuous
            attr-list
            attr-colour
            get-attr-type
            get-attr-range
            get-attr-name
            get-attr-home-val
            intensity?
            continuous-attribute?
            colour-attribute?

            <starlet-state>
            make-empty-state
            state-for-each
            state-map
            clear-state!
            print-state
            state-source
            set-in-state!
            state-find
            current-state
            at
            apply-state
            show-state
            lighting-state
            blackout
            register-state!

            start-ola-output
            scanout-freq
            percent->dmxval8
            percent->dmxval16
            hirestime
            value->number

            programmer-state
            sel
            current-value
            selection-hook

            scale-to-range))


(define-class <fixture-attribute> (<object>)
  (name
    #:init-form (error "Attribute name must be specified")
    #:init-keyword #:name
    #:getter get-attr-name)

  (range
    #:init-value '()
    #:init-keyword #:range
    #:getter get-attr-range)

  (type
    #:init-value 'continuous
    #:init-keyword #:type
    #:getter get-attr-type)

  (home-value
    #:init-value 0
    #:init-keyword #:home-value
    #:getter attr-home-value))


(define-class <fixture> (<object>)
  (name
    #:init-form (error "Fixture name must be specified")
    #:init-keyword #:name
    #:getter get-fixture-name)

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
    #:setter set-fixture-friendly-name!)

  (scanout-func
    #:init-value (lambda (universe start-addr value set-dmx) #f)
    #:init-keyword #:scanout-func
    #:getter get-scanout-func))


(define (get-attributes f)
  (slot-ref f 'attributes))


(define (fixture? f)
  (is-a? f <fixture>))


;; A "state" is just a thin wrapper around a hash table
;; of (fixture . attribute)  -->  value
(define-class <starlet-state> (<object>)
  (hash-table
    #:init-form (make-hash-table)
    #:getter get-state-hash-table)
  (name
    #:init-value #f
    #:init-keyword #:name
    #:getter get-state-name
    #:setter set-state-name!))


(define-method (set-in-state! (state <starlet-state>)
                              (fix <fixture>)
                              (attr <symbol>)
                              value)
  (hash-set! (get-state-hash-table state)
             (cons fix attr)
             value))


;; List of fixtures and home state (must remain consistent)
(define fixtures (make-atomic-box '()))

;; List of states being scanned out
(define state-list (make-atomic-box '()))

;; The state used to build a new scene for recording
(define programmer-state (make <starlet-state>))


(define (make-empty-state)
  (make <starlet-state>))


(define (find-attr fix attr-name)
  (find (lambda (a)
          (eq? (get-attr-name a)
               attr-name))
        (slot-ref fix 'attributes)))


(define (get-attr-home-val fix attr)
  (let ((attr-obj (find-attr fix attr)))
    (if attr-obj
        (attr-home-value attr-obj)
        'fixture-does-not-have-attribute)))


(define (blackout state)
  (state-for-each
    (lambda (fix attr val)
      (when (intensity? attr)
        (set-in-state! state fix attr 0.0)))
    state))


;; Set a single attribute to home position
(define (home-attr! state fix attr)
  (set-in-state! state
                 fix
                 attr
                 (get-attr-home-val fix attr)))


(define (copy-state state)
  (let ((new-state (make-empty-state)))
    (state-for-each (lambda (fix attr val)
                      (set-in-state! new-state
                                     fix
                                     attr
                                     val))
                    state)
    new-state))


(define (intensity? a)
  (eq? 'intensity a))


(define (continuous-attribute? aobj)
  (eq? 'continuous
       (get-attr-type aobj)))


(define (colour-attribute? aobj)
  (eq? 'colour
       (get-attr-type aobj)))


(define (append-or-replace-named-state orig-list name new-state)
  (let ((new-list (map (lambda (st)
                         (if (eq? (get-state-name st) name)
                             (begin
                               new-state)
                             st))
                       orig-list)))

    ;; If there is no state with this name in the list,
    ;; the replacement above will have no effect.
    ;; Check again and add in the normal way if so.
    (if (find (lambda (st) (eq? (get-state-name st)
                                name))
              new-list)
        new-list
        (append orig-list (list new-state)))))


(define* (register-state! new-state
                          #:key (unique-name #f))
  (if unique-name
      (begin (set-state-name! new-state unique-name)
             (atomic-box-set! state-list
                              (append-or-replace-named-state (atomic-box-ref state-list)
                                                             unique-name
                                                             new-state)))
      (atomic-box-set! state-list
                       (append (atomic-box-ref state-list)
                               (list new-state)))))

;; Patch a new fixture
(define* (patch-real name
                     class
                     start-addr
                     #:key (universe 0) (friendly-name "Fixture"))
  (let ((new-fixture (make class
                           #:name name
                           #:sa start-addr
                           #:uni universe
                           #:friendly-name friendly-name)))
    (atomic-box-set! fixtures (cons new-fixture
                                    (atomic-box-ref fixtures)))
    new-fixture))


(define-syntax patch-fixture!
  (syntax-rules ()
    ((_ name stuff ...)
     (define name (patch-real (quote name) stuff ...)))))


;; Helper functions for scanout functions
(define (round-dmx a)
  (inexact->exact
   (min 255 (max 0 (round a)))))

(define (scale-to-range val orig-range dest-range)

  (define (range r)
    (- (cadr r) (car r)))

  (+ (car dest-range)
     (* (range dest-range)
        (/ (- val (car orig-range))
           (range orig-range)))))

(define (percent->dmxval8 val)
  (round-dmx
   (scale-to-range val '(0 100) '(0 255))))

(define (percent->dmxval16 val)
  (scale-to-range val '(0 100) '(0 65535)))

(define (msb val)
  (round-dmx (euclidean-quotient val 256)))

(define (lsb val)
  (round-dmx (euclidean-remainder val 256)))


(define (state-for-each func state)
  (hash-for-each (lambda (key value)
                   (func (car key)
                         (cdr key)
                         value))
                 (get-state-hash-table state)))


(define (state-find fix attr state)
  (hash-ref (get-state-hash-table state)
            (cons fix attr)
            'no-value))

(define (state-map func state)
  (hash-map->list (lambda (key value)
                    (func (car key)
                          (cdr key)
                          value))
                  (get-state-hash-table state)))


(define (apply-state state)
  "Apply the contents of 'state' to the current state, on top of the \
pre-existing contents."
  (state-for-each at state))


(define (show-state state)
  "Clear the current state, and apply the contents of 'state'"
  (clear-state! (current-state))
  (state-for-each at state))


;; Coerce something from a state object into a number for scanout
(define (value->number val time)
  (if (procedure? val)
      (value->number (val time) time)
      val))


(define (clear-state! state)
  (hash-clear! (get-state-hash-table state)))


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


(define (ensure-number value irritating)
  (unless (number? value)
    (raise-exception (make-exception
                       (make-exception-with-message "Value is not a number")
                       (make-exception-with-irritants irritating)))))


(define scanout-freq 0)

(define-generic scanout-fixture)

(define (scanout-loop ola-uri ola-socket start-time count)

  (let ((universes '()))

    ;; Helper function for scanout functions to set individual DMX values
    (define (set-dmx universe addr value)
      (ensure-number value (list universe addr value))

      ;; Create DMX array for universe if it doesn't exist already
      (unless (assq universe universes)
        (set! universes (acons universe
                               (make-u8vector 512 0)
                               universes)))

      (u8vector-set! (assq-ref universes universe)
                     (- addr 1)                   ; u8vector-set indexing starts from zero
                     (round-dmx value)))

    (for-each
      (lambda (fix)

        (let ((univ (get-fixture-universe fix))
              (addr (get-fixture-addr fix)))

          ;; Helper function to get a value for this
          ;; fixture in the current state
          (define (get-attr attr-name)
            (current-value fix attr-name (hirestime)))

          ;; Helper function to set 8-bit DMX value
          (define (set-chan relative-channel-number value)
            (ensure-number value (list fix relative-channel-number value))
            (set-dmx univ (+ addr relative-channel-number -1) value))

          ;; Helper function to set 16-bit DMX value
          (define (set-chan-16bit relative-channel-number value)
            (ensure-number value (list fix relative-channel-number value))
            (set-chan relative-channel-number (msb value))
            (set-chan (+ relative-channel-number 1) (lsb value)))

          (scanout-fixture fix get-attr set-chan set-chan-16bit)))

      (atomic-box-ref fixtures))

    ;; Send everything to OLA
    (for-each (lambda (a)
                (send-to-ola ola-uri ola-socket a))
              universes)

    (usleep 10000)

    ;; Update scanout rate every 1000 cycles
    (if (eq? count 100)
        (begin
          (set! scanout-freq
            (exact->inexact (/ 100
                               (- (hirestime) start-time))))
          (scanout-loop ola-uri ola-socket (hirestime) 0))
        (scanout-loop ola-uri ola-socket start-time (+ count 1)))))

(define ola-thread #f)

(define (start-ola-output)
  (unless ola-thread
    (let* ((ola-uri (build-uri 'http
                               #:host "127.0.0.1"
                               #:port 9090
                               #:path "/set_dmx"))
           (ola-socket (open-socket-for-uri ola-uri))
           (start-time (hirestime)))

      (set! ola-thread
        (begin-thread
          (with-exception-handler
            (lambda (exn)
              (display "Error in OLA output thread:\n")
              (set! ola-thread #f)
              (backtrace)
              (raise-exception exn))
            (lambda ()
              (scanout-loop ola-uri ola-socket start-time 0))
            #:unwind? #f))))))


(define (state-has-fix-attr fix attr tnow state)
  (let ((val (state-find fix attr state)))
    (if (eq? 'no-value val)
        #f
        (not (eq? 'no-value (value->number val tnow))))))

(define (first-val fix attr tnow state-list)
  (let ((first-state (find (lambda (state)
                             (state-has-fix-attr fix attr tnow state))
                           state-list)))
    (if first-state
        (state-find fix attr first-state)
        'no-value)))

(define (current-value fix attr-name tnow)
  (let  ((programmer-val (state-find fix attr-name programmer-state)))
    (if (eq? 'no-value programmer-val)

        ;; Look in the states
        (if (intensity? attr-name)

            ;; HTP for intensity
            (fold (lambda (state prev)
                    (let ((val (state-find fix attr-name state)))
                      (if (eq? 'no-value val)
                          prev
                          (let ((real-val (value->number val tnow)))
                            (if (eq? 'no-value real-val)
                                prev
                                (max real-val prev))))))
                  0.0
                  (atomic-box-ref state-list))

            ;; Priority order for everything else
            (let ((val (first-val fix attr-name tnow (atomic-box-ref state-list))))
              (if (eq? 'no-value val)
                  (get-attr-home-val fix attr-name)
                  (value->number val tnow))))

        ;; Use programmer value, if we have it
        programmer-val)))


(define-syntax attr-continuous
  (syntax-rules ()
    ((_ attr-name attr-range attr-home-value)
     (make <fixture-attribute>
       #:name attr-name
       #:range attr-range
       #:type 'continuous
       #:home-value attr-home-value))))


(define-syntax attr-list
  (syntax-rules ()
    ((_ attr-name attr-allowed-values attr-home-value)
     (make <fixture-attribute>
       #:name attr-name
       #:range attr-allowed-values
       #:type 'list
       #:home-value attr-home-value))))


(define-syntax attr-colour
  (syntax-rules ()
    ((_ attr-name attr-home-value)
     (make <fixture-attribute>
       #:name attr-name
       #:type 'colour
       #:home-value attr-home-value))))


(define current-state (make-parameter programmer-state))


(define-syntax lighting-state
  (syntax-rules ()
    ((_ body ...)
     (parameterize ((current-state (make-empty-state)))
       body ...
       (current-state)))))


(define (partition3 pred1 pred2 input)
  (receive (output1 others)
           (partition pred1 input)
           (receive (output2 others)
                    (partition pred2 others)
                    (values output1 output2 others))))


(define (set-fixtures fixtures attr-name value)
  (for-each (lambda (fix)
              (set-in-state! (current-state)
                             fix
                             (car attr-name)
                             (car value)))
            fixtures))


;; (at <fixtures/groups> [<attribute>] <level> [<attribute> <level>...])
;; (at fix1 100)                      <-- Set intensity of single fixture
;; (at fix1 'intensity 100)           <-- Explicit attribute name
;; (at fix1 fix2 100)                 <-- Multiple fixtures
;; (at fix1 fix2 'pan 36)             <-- Multiple fixtures + explicit attribute
;; (at group1 fix1 'intensity 100)    <-- Groups can be used instead of fixtures
;; (at fix1 100 'pan 36)              <-- Set multiple attributes
;; NB Can't set multiple fixtures and attributes: (at fix1 'pan 35 fix2 'tilt 22)

(define (at . args)
  (receive (fixtures attr-name value)
           (partition3 fixture? symbol? (flatten-sublists args))
           (cond

             ((nil? value)
              (error "at: Value not specified"))

             ((or (more-than-one value)
                  (more-than-one attr-name))
              (error "at: Only one attribute or value name"))

             ((and (nil? fixtures)
                   (nil? attr-name))
              (if (nil? selection)
                  'no-fixtures-selected
                  (set-fixtures selection '(intensity) value)))

             ((nil? attr-name)
              (set-fixtures fixtures '(intensity) value))

             ((nil? fixtures)
              (if (nil? selection)
                  'no-fixtures-selected
                  (set-fixtures selection attr-name value)))

             (else
               (set-fixtures fixtures attr-name value)))))


(define selection-hook (make-hook 1))

(define selection '())


(define (sel . fixture-list)
  (if (nil? fixture-list)
      (set! selection '())
      (if (not (car fixture-list))
          (set! selection '())
          (set! selection (flatten-sublists fixture-list))))
  (run-hook selection-hook selection))


(define (print-state a)
  (pretty-print (state-source a)))


(define (state-source a)
  (cons 'lighting-state
        (state-map (lambda (fix attr val)
                     (list 'at
                           (get-fixture-name fix)
                           (list 'quote attr)
                           val))
                   a)))
