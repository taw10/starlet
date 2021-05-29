;;
;; starlet/fixture-library/chauvet.scm
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
(define-module (starlet fixture-library chauvet)
  #:use-module (oop goops)
  #:use-module (starlet fixture)
  #:export (<chauvet-mav2-32ch>))


(define-class <chauvet-mav2-32ch> (<fixture>)
  (attributes
   #:init-form (list
                (attr-continuous 'intensity '(0 100) 0)
                (attr-continuous 'pan '(0 540) 270)
                (attr-continuous 'tilt '(0 270) 135)
                (attr-continuous 'cyan '(0 100) 0)
                (attr-continuous 'magenta '(0 100) 0)
                (attr-continuous 'yellow '(0 100) 0))))


(define-method (scanout-fixture (fixture <chauvet-mav2-32ch>)
                                get-attr set-chan set-chan-16bit)

  (set-chan-16bit 1 (get-attr 'pan) 540)
  (set-chan-16bit 3 (get-attr 'tilt) 270)
  (set-chan-16bit 6 (get-attr 'intensity) 100)

  (set-chan 10 (percent->dmxval (get-attr 'cyan)))
  (set-chan 11 (percent->dmxval (get-attr 'magenta)))
  (set-chan 12 (percent->dmxval (get-attr 'yellow)))

  (set-chan 8 255))
