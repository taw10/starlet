(define-module (starlet fixture-library arduino)
  #:use-module (oop goops)
  #:use-module (starlet base)
  #:use-module (starlet colours)
  #:export (<arduino-dmx-thing>))


;; This fixture class drives the RGB LED on the Arduino DMX shield
;; as described by Matthias Hertel at http://www.mathertel.de/Arduino/DMXShield.aspx


(define-class <arduino-dmx-thing> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0)
                (attr-colour 'colour white))))


(define-method (scanout-fixture (fixture <arduino-dmx-thing>)
                                get-attr set-chan8 set-chan16)

  (let ((intensity (get-attr 'intensity))
        (rgb (colour-as-rgb (get-attr 'colour))))
    (set-chan8 1 (percent->dmxval8 (* intensity 0.01 (car rgb))))
    (set-chan8 2 (percent->dmxval8 (* intensity 0.01 (cadr rgb))))
    (set-chan8 3 (percent->dmxval8 (* intensity 0.01 (caddr rgb))))))

