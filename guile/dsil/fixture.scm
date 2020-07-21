(define-module (dsil fixture)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:export (<fixture> <fixture-attribute>
             start-ola-output patch-fixture
             set-attr!
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
    #:setter set-attr-home-value!)

  (value
    #:init-value 0
    #:getter get-attr-value
    #:setter set-attr-value!))


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


;; List of fixtures
(define patched-fixture-list (make-atomic-box '()))


(define (get-attributes fix)
  (slot-ref fix 'attributes))


;; Set a single attribute to home position
(define (home-attr! attr)
  (set-attr-value! attr
                   (get-attr-home-value attr)))


;; Set all attributes of a fixture to home position
(define (home-all! fix)
  (for-each home-attr!
            (get-attributes fix)))


(define (find-attr fix attr-name)
  (find (lambda (a)
          (eq? (get-attr-name a)
               attr-name))
        (get-attributes fix)))


(define (set-attr! fix attr-name value)
  (let ((attr (find-attr fix attr-name)))
    (when attr (set-attr-value! attr value))))


(define* (patch-fixture class
                        start-addr
                        #:key (universe 1) (friendly-name "Fixture"))
  (let ((new-fixture (make class
                       #:sa start-addr
                       #:uni universe
                       #:friendly-name friendly-name)))
    (home-all! new-fixture)
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

          ;; Scan out all fixtures
          (for-each (lambda (fix)

                      ;; Scan out one fixture
                      (for-each (lambda (attr)

                                  ;; Scan out one attribute
                                  (let ((trans (get-attr-translator attr)))
                                    (trans (get-fixture-universe fix)
                                           (get-fixture-addr fix)
                                           (get-attr-value attr)
                                           set-dmx)))
                                (get-attributes fix)))

                    (atomic-box-ref patched-fixture-list))

          ;; Send everything to OLA
          (for-each (lambda (a)
                      (send-to-ola ola-uri ola-socket a))
                    universes))

        (yield)
        (scanout-loop)))))
