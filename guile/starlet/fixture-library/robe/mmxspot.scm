;;
;; starlet/fixture-library/robe/mmxspot/mode1.scm
;;
;; Copyright © 2020-2021 Thomas White <taw@bitwiz.org.uk>
;;
;; This file is part of Starlet.
;;
;; Starlet is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(define-module (starlet fixture-library robe mmxspot)
  #:use-module (oop goops)
  #:use-module (starlet fixture)
  #:use-module (starlet colours)
  #:export (<robe-mmxspot-mode1>))


(define-fixture

  <robe-mmxspot-mode1>

  (fixture-attributes
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
    (attr-continuous 'cto '(3200 6900) 6900))

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
