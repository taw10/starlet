(define-module (starlet fixture-library robe)
  #:use-module (oop goops)
  #:use-module (starlet base)
  #:use-module (starlet colours)
  #:export (<robe-dl7s-mode1>
            <robe-mmxwashbeam-mode1>
            <robe-mmxspot-mode1>))


(define-class <robe-dl7s-mode1> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0)
                (attr-continuous 'pan '(0 540) 270)
                (attr-continuous 'tilt '(0 270) 135)
                (attr-list 'strobe '(#t #f) #f)
                (attr-list 'prism '(#t #f) #f)
                (attr-list 'tungsten-watts-emulation '(750 1000 1200 2000 2500 #f) #f)
                (attr-colour 'colour white)
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

  (let ((cmy (colour-as-cmy (get-attr 'colour))))
    (set-chan8 9 (percent->dmxval8 (car cmy)))
    (set-chan8 11 (percent->dmxval8 (cadr cmy)))
    (set-chan8 13 (percent->dmxval8 (caddr cmy)))))


(define-class <robe-mmxwashbeam-mode1> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0)
                (attr-continuous 'pan '(0 540) 270)
                (attr-continuous 'tilt '(0 270) 135)
                (attr-list 'strobe '(#t #f) #f)
                (attr-list 'colwheel '(#f red blue orange green amber uv) #f)
                (attr-list 'gobo '(#f iris gobo1 gobo2 gobo3 gobo4 gobo5 gobo6) #f)
                (attr-list 'beamtype '(beam beamwash beamwashext) 'beam)
                (attr-colour 'colour white)
                (attr-continuous 'zoom '(0 100) 0)
                (attr-continuous 'focus '(0 100) 0)
                (attr-continuous 'barndoor-rot '(0 180) 90)
                (attr-continuous 'barndoor1 '(0 180) 0)
                (attr-continuous 'barndoor2 '(0 100) 0)
                (attr-continuous 'barndoor3 '(0 100) 0)
                (attr-continuous 'barndoor4 '(0 100) 0))))


(define-method (scanout-fixture (fixture <robe-mmxwashbeam-mode1>)
                                get-attr set-chan8 set-chan16)

  (set-chan16 33 (percent->dmxval16 (get-attr 'intensity)))

  (set-chan16 1 (scale-to-range (get-attr 'pan) '(0 540) '(0 65535)))
  (set-chan16 3 (scale-to-range (get-attr 'tilt) '(0 270) '(0 65535)))

  (set-chan8 32 (if (get-attr 'strobe) 70 255))

  (set-chan16 19 (percent->dmxval16 (get-attr 'zoom)))
  (set-chan16 21 (percent->dmxval16 (get-attr 'focus)))

  ;;(set-chan 24 (number->dmxval (get-attr 'barndoor-rot) '(0 180)))
  (set-chan8 25 (percent->dmxval8 (get-attr 'barndoor1)))
  (set-chan8 26 (percent->dmxval8 (get-attr 'barndoor2)))
  (set-chan8 27 (percent->dmxval8 (get-attr 'barndoor3)))
  (set-chan8 28 (percent->dmxval8 (get-attr 'barndoor4)))

  (set-chan8 7 (assv-ref '((#f . 0)
                           (red . 18)
                           (blue . 37)
                           (orange . 55)
                           (green . 73)
                           (amber . 91)
                           (uv . 110))
                         (get-attr 'colwheel)))

  (set-chan8 15 (assv-ref '((#f . 0)
                            (iris . 5)
                            (gobo1 . 10)
                            (gobo2 . 14)
                            (gobo3 . 18)
                            (gobo4 . 22)
                            (gobo5 . 26)
                            (gobo6 . 30))
                          (get-attr 'gobo)))

  (set-chan8 18 (assv-ref '((beam . 0)
                            (beamwash . 35)
                            (beamwashext . 45))
                          (get-attr 'beamtype)))

  (let ((cmy (colour-as-cmy (get-attr 'colour))))
    (set-chan8 9 (percent->dmxval8 (car cmy)))
    (set-chan8 10 (percent->dmxval8 (cadr cmy)))
    (set-chan8 11 (percent->dmxval8 (caddr cmy)))))


(define-class <robe-mmxspot-mode1> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0)
                (attr-continuous 'pan '(0 540) 270)
                (attr-continuous 'tilt '(0 270) 135)
                (attr-list 'colwheel '(#f red blue orange green amber uv) #f)
                (attr-list 'prism '(#t #f) #f)
                (attr-list 'strobe '(#f #t random zap) #f)
                (attr-continuous 'strobe-speed '(0 100) 50)
                (attr-colour 'colour white)
                (attr-continuous 'iris '(0 100) 0)
                (attr-continuous 'zoom '(0 100) 0)
                (attr-continuous 'focus '(0 100) 0)
                (attr-continuous 'hotspot '(0 100) 0)
                (attr-continuous 'frost '(0 100) 0)
                (attr-continuous 'cto '(3200 6900) 6900))))


(define-method (scanout-fixture (fixture <robe-mmxspot-mode1>)
                                get-attr set-chan8 set-chan16)

  (set-chan16 37 (percent->dmxval16 (get-attr 'intensity)))

  (set-chan16 1 (scale-to-range (get-attr 'pan) '(0 540) '(0 65535)))

  (set-chan16 3 (scale-to-range (get-attr 'tilt) '(0 270) '(0 65535)))

  (set-chan16 28 (scale-to-range (get-attr 'iris) '(0 100) '(0 45567)))
  (set-chan16 30 (percent->dmxval16 (get-attr 'zoom)))
  (set-chan16 32 (percent->dmxval16 (get-attr 'focus)))

  (set-chan8 36
             (let ((strb (get-attr 'strobe))
                   (spd (get-attr 'strobe-speed)))
               (cond
                 ((eq? strb #t) (scale-to-range spd '(0 100) '(64 95)))
                 ((eq? strb 'random) (scale-to-range spd '(0 100) '(192 223)))
                 ((eq? strb 'zap) (scale-to-range spd '(0 100) '(160 191)))
                 (else 255))))

  (set-chan8 25 (if (get-attr 'prism) 20 0))

  (set-chan8 7 (assv-ref '((#f . 0)
                           (red . 18)
                           (blue . 37)
                           (orange . 55)
                           (green . 73)
                           (amber . 91)
                           (uv . 110))
                         (get-attr 'colwheel)))

  (let ((cmy (colour-as-cmy (get-attr 'colour))))
    (set-chan8 9 (percent->dmxval8 (car cmy)))
    (set-chan8 10 (percent->dmxval8 (cadr cmy)))
    (set-chan8 11 (percent->dmxval8 (caddr cmy))))

  (set-chan8 35 (percent->dmxval8 (get-attr 'hotspot)))
  (set-chan8 12 (scale-to-range (get-attr 'cto) '(3200 6900) '(0 255)))
  (set-chan8 27 (scale-to-range (get-attr 'frost) '(0 100) '(0 179))))
