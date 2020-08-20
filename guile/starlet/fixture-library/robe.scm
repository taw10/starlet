(define-module (starlet fixture-library robe)
  #:use-module (oop goops)
  #:use-module (starlet base)
  #:export (<robe-dl7s-mode1>))


(define-class <robe-dl7s-mode1> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0)
                (attr-continuous 'pan '(0 540) 270)
                (attr-continuous 'tilt '(0 270) 135)
                (attr-boolean 'strobe #f)
                (attr-boolean 'prism #f)
                (attr-list 'tungsten-watts-emulation '(750 1000 1200 2000 2500 #f) #f)
                (attr-continuous 'cyan '(0 100) 0)
                (attr-continuous 'magenta '(0 100) 0)
                (attr-continuous 'yellow '(0 100) 0)
                (attr-continuous 'colour-temperature-correction '(2700 8000) 8000)
                (attr-continuous 'green-correction '(-100 100) 0))))


(define-method (scanout-fixture (fixture <robe-dl7s-mode1>)
                                get-attr
                                set-dmx)

  (define (set-chan relative-channel-number value)
    (set-dmx (get-fixture-universe fixture)
             (+ (get-fixture-addr fixture)
                (- relative-channel-number 1))
             value))

  (define (set-chan-16bit relative-channel-number value max-value)
    (let ((val16 (* value (/ 65535 max-value))))
      (set-chan relative-channel-number (msb val16))
      (set-chan (+ relative-channel-number 1) (lsb val16))))

  (set-chan-16bit 50 (get-attr 'intensity) 100)

  (set-chan-16bit 1 (get-attr 'pan) 540)
  (set-chan-16bit 3 (get-attr 'tilt) 270)

  (set-chan 49 (if (get-attr 'strobe) 95 32))

  (set-chan 28 (if (get-attr 'prism) 50 0))

  (set-chan 7 (assv-ref '((750 . 82)
                          (1000 . 88)
                          (1200 . 92)
                          (2000 . 97)
                          (2500 . 102)
                          (#f . 107))
                        (get-attr 'tungsten-watts-emulation)))

  (set-chan-16bit 9 (get-attr 'cyan) 100)
  (set-chan-16bit 11 (get-attr 'magenta) 100)
  (set-chan-16bit 13 (get-attr 'yellow) 100))
