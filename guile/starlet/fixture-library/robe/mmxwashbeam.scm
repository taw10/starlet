;;
;; starlet/fixture-library/robe/mmxwashbeam.scm
;;
;; Copyright © 2020-2022 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet fixture-library robe mmxwashbeam)
  #:use-module (starlet scanout)
  #:use-module (starlet fixture)
  #:use-module (starlet attributes)
  #:use-module (starlet utils)
  #:use-module (starlet colours)
  #:export (<robe-mmxwashbeam-mode1>))


(define-fixture

  <robe-mmxwashbeam-mode1>

  (fixture-attributes
    (attr-continuous intensity '(0 100) 0)
    (attr-continuous pan '(0 540) 270)
    (attr-continuous tilt '(0 270) 135)
    (attr-list strobe '(#t #f) #f)
    (attr-list colwheel '(#f red blue orange green amber uv) #f)
    (attr-list gobo '(#f iris gobo1 gobo2 gobo3 gobo4 gobo5 gobo6) #f)
    (attr-list beamtype '(beam beamwash beamwashext) 'beam)
    (attr-colour colour white)
    (attr-continuous zoom '(0 100) 0)
    (attr-continuous focus '(0 100) 0)
    (attr-continuous barndoor-rot '(0 180) 90)
    (attr-continuous barndoor1 '(0 180) 0)
    (attr-continuous barndoor2 '(0 100) 0)
    (attr-continuous barndoor3 '(0 100) 0)
    (attr-continuous barndoor4 '(0 100) 0))

  (set-chan16 33 (percent->dmxval16 (get-attr intensity)))

  (set-chan16 1 (scale-to-range (get-attr pan) '(0 540) '(0 65535)))
  (set-chan16 3 (scale-to-range (get-attr tilt) '(0 270) '(0 65535)))

  (set-chan8 32 (if (get-attr strobe) 70 255))

  (set-chan16 19 (percent->dmxval16 (get-attr zoom)))
  (set-chan16 21 (percent->dmxval16 (get-attr focus)))

  ;;(set-chan 24 (number->dmxval (get-attr barndoor-rot) '(0 180)))
  (set-chan8 25 (percent->dmxval8 (get-attr barndoor1)))
  (set-chan8 26 (percent->dmxval8 (get-attr barndoor2)))
  (set-chan8 27 (percent->dmxval8 (get-attr barndoor3)))
  (set-chan8 28 (percent->dmxval8 (get-attr barndoor4)))

  (set-chan8 7 (assv-ref '((#f . 0)
                           (red . 18)
                           (blue . 37)
                           (orange . 55)
                           (green . 73)
                           (amber . 91)
                           (uv . 110))
                         (get-attr colwheel)))

  (set-chan8 15 (assv-ref '((#f . 0)
                            (iris . 5)
                            (gobo1 . 10)
                            (gobo2 . 14)
                            (gobo3 . 18)
                            (gobo4 . 22)
                            (gobo5 . 26)
                            (gobo6 . 30))
                          (get-attr gobo)))

  (set-chan8 18 (assv-ref '((beam . 0)
                            (beamwash . 35)
                            (beamwashext . 45))
                          (get-attr beamtype)))

  (let ((cmy (colour-as-cmy (get-attr colour))))
    (set-chan8 9 (percent->dmxval8 (car cmy)))
    (set-chan8 10 (percent->dmxval8 (cadr cmy)))
    (set-chan8 11 (percent->dmxval8 (caddr cmy)))))
