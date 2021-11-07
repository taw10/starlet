;;
;; starlet/fixture-library/tadm/led-foh.scm
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
(define-module (starlet fixture-library tadm led-foh)
  #:use-module (oop goops)
  #:use-module (starlet fixture)
  #:use-module (starlet colours)
  #:export (<tadm-led-foh>))


(define-class <tadm-led-foh> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0)
                (attr-colour 'colour white))))


(define-method (scanout-fixture (fixture <tadm-led-foh>)
                                get-attr set-chan8 set-chan16)

  (let ((intensity (get-attr 'intensity))
        (rgbw (colour-as-rgbw (get-attr 'colour))))
    (set-chan8 1 (percent->dmxval8 intensity))
    (set-chan8 2 255)
    (set-chan8 3 (percent->dmxval8 (car rgbw)))
    (set-chan8 4 (percent->dmxval8 (cadr rgbw)))
    (set-chan8 5 (percent->dmxval8 (caddr rgbw)))
    (set-chan8 6 (percent->dmxval8 (cadddr rgbw)))))

