;;
;; starlet/fixture-library/robe/dl7s/mode1.scm
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
(define-module (starlet fixture-library robe dl7s mode1)
  #:use-module (oop goops)
  #:use-module (starlet fixture)
  #:use-module (starlet colours)
  #:export (<robe-dl7s-mode1>))


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
