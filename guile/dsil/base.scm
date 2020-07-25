(define-module (dsil base)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:export (<fixture> <fixture-attribute>
             start-ola-output patch-fixture
             set-attr! home-attr! home-all! blackout
             make-workspace
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


(define-class <workspace> (<object>)

  (attributes
   #:init-form (make-atomic-box '())
   #:getter get-workspace-attributes
   #:setter set-workspace-attributes!)

  (priority
   #:init-value 0
   #:init-keyword #:priority
   #:getter get-workspace-priority
   #:setter set-workspace-priority!))


(define-class <state-assignment> (<object>)

  (fixture
    #:init-value #f
    #:init-keyword #:fixture
    #:getter get-assignment-fixture)

  (attribute
    #:init-value #f
    #:init-keyword #:attribute
    #:getter get-assignment-attribute)

  (value
    #:init-value #f
    #:init-keyword #:value-func
    #:getter get-assignment-value))


;; List of fixtures
(define patched-fixture-list (make-atomic-box '()))

;; List of workspaces
(define base-workspace (make <workspace>))
(define workspace-list (make-atomic-box (list base-workspace)))


(define (make-assignment fix attr value)
  (make <state-assignment>
    #:fixture fix
    #:attribute attr
    #:value value))


(define (add-attribute workspace
                       fix
                       attr
                       value)
  (letrec* ((attr-box (get-workspace-attributes workspace))
            (workspace-attrs (atomic-box-ref attr-box)))
    ;; FIXME: Should be compare-and-swap
    (atomic-box-set! attr-box
                     (cons (make-assignment fix attr value)
                           workspace-attrs))))


(define (get-fixture-attributes fix)
  (slot-ref fix 'attributes))


;; Set a single attribute to home position
(define (home-attr! workspace fix attr)
  (add-attribute workspace
                 fix
                 attr
                 (get-attr-home-value attr)))


;; Set all attributes of a fixture to home position
(define (home-all! workspace fix)
  (for-each (lambda (attr)
              (home-attr! workspace fix attr))
            (get-fixture-attributes fix)))


;; Set the intensity of all patched fixtures to zero
(define (blackout workspace)
  (for-each (lambda (fix)
              (set-attr! workspace fix 'intensity 0))
            (atomic-box-ref patched-fixture-list)))


(define (find-attr fix attr-name)
  (find (lambda (a)
          (eq? (get-attr-name a)
               attr-name))
        (get-fixture-attributes fix)))


(define (make-workspace)
  (let ((new-workspace (make <workspace>)))
    (atomic-box-set! workspace-list
                     (cons new-workspace
                           (atomic-box-ref workspace-list)))
    new-workspace))


;; Set an attribute
(define (set-attr! workspace fix attr-name value)
  (let ((attr (find-attr fix attr-name)))
    (when attr (add-attribute workspace fix attr value))))



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



;; Combine states
(define (combine-states list-of-states)
  (let (()))
  (fold
    (lambda (assignment-to-add combined-state))
    '() (apply append list-of-states)))



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

(define (start-ola-output)
  (letrec* ((ola-uri (build-uri 'http
                                #:host "127.0.0.1"
                                #:port 9090
                                #:path "/set_dmx"))
            (ola-socket (open-socket-for-uri ola-uri)))

    (begin-thread
     (let scanout-loop ()

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

          ;; Combine all the workspaces into one
          (let ((combined-workspace
                 (combine-workspaces
                  (atomic-box-ref workspace-list))))

            ;; Scan out all attributes of the combined workspace
            (for-each (lambda (fix-attr-val)

                        ;; Scan out one attribute
                        (let ((trans (get-attr-translator (cadr fix-attr-val))))
                          (trans (get-fixture-universe (car fix-attr-val))
                                 (get-fixture-addr (car fix-attr-val))
                                 (cddr fix-attr-val)
                                 set-dmx)))

                      (get-workspace-attributes combined-workspace)))

          ;; Send everything to OLA
          (for-each (lambda (a)
                      (send-to-ola ola-uri ola-socket a))
                    universes))

        (yield)
        (scanout-loop)))))
