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
                                get-attr set-chan8 set-chan16)

  (set-chan16 50 (percent->dmxval16 (get-attr 'intensity)))

  (set-chan16 1 (scale-to-range (get-attr 'pan) '(0 540) '(0 65535)))
  (set-chan16 3 (scale-to-range (get-attr 'tilt) (0 270) '(0 65535)))

  (set-chan8 49 (if (get-attr 'strobe) 95 32))

  (set-chan8 28 (if (get-attr 'prism) 50 0))

  (set-chan8 7 (assv-ref '((750 . 82)
                           (1000 . 88)
                           (1200 . 92)
                           (2000 . 97)
                           (2500 . 102)
                           (#f . 107))
                         (get-attr 'tungsten-watts-emulation)))

  (set-chan16 9 (percent->dmxval16 (get-attr 'cyan)))
  (set-chan16 11 (percent->dmxval16 (get-attr 'magenta)))
  (set-chan16 13 (percent->dmxval16 (get-attr 'yellow))))
