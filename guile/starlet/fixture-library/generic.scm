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
      #:home-value 0
      #:translator (lambda (universe start-addr value set-dmx)
                     (set-dmx universe start-addr
                              (percent->dmxval value)))))))
