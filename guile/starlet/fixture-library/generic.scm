(define-module (starlet fixture-library generic)
  #:use-module (oop goops)
  #:use-module (starlet base)
  #:export (<generic-dimmer>))

(define-class <generic-dimmer> (<fixture>)

  (attributes
   #:init-form
   (list

    (make <fixture-attribute>
      #:name 'intensity
      #:range '(0 100)
      #:type 'continuous
      #:home-value 0))))


(define-method (scanout-fixture (fixture <generic-dimmer>)
                                get-attr
                                set-dmx)

    ;; Set DMX value for intensity
    (set-dmx (get-fixture-universe fixture)
             (get-fixture-addr fixture)
             (percent->dmxval (get-attr 'intensity))))
