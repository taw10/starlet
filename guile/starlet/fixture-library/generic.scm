(define-module (starlet fixture-library generic)
  #:use-module (oop goops)
  #:use-module (starlet base)
  #:export (<generic-dimmer>
            generic-rgb))

(define-class <generic-dimmer> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0))))


(define-method (scanout-fixture (fixture <generic-dimmer>)
                                get-attr set-chan set-chan-16bit)

  ;; Set DMX value for intensity
  (set-chan 1 (percent->dmxval (get-attr 'intensity))))


(define (chan->attr chan)
  (attr-continuous chan '(0 100) 0))


(define (generic-rgb chans)

  (let ((new-class (make-class
                    (list <fixture>)
                    (list (cons 'attributes
                                (list #:init-thunk
                                      (lambda ()
                                        (map chan->attr chans)))))
                    #:name 'generic-rgb)))

    (add-method!
     scanout-fixture
     (method ((fix new-class) get-attr set-chan set-chan-16bit)
             (for-each

              (lambda (chan offset)

                (cond

                 ((eq? chan '0)
                  (set-chan offset 0))

                 ((eq? chan 'FL)
                  (set-chan offset 255))

                 (else (set-chan offset
                                 (percent->dmxval
                                  (get-attr chan))))))

              chans (iota (length chans) 1))))

    new-class))
