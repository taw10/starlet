;;
;; starlet/fixture-library/stairville/z120m.scm
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
(define-module (starlet fixture-library stairville z120m)
  #:use-module (starlet scanout)
  #:use-module (starlet fixture)
  #:use-module (starlet attributes)
  #:use-module (starlet utils)
  #:use-module (starlet colours)
  #:export (<stairville-z120m-6ch>))


(define (colour-as-rgbw-weirdness col weirdness)
  (let ((rgb (colour-as-rgb col)))
    (let ((w (* (- 1 weirdness) (apply min rgb))))
      (list (- (red rgb) w)
            (- (green rgb) w)
            (- (blue rgb) w)
            w))))


(define-fixture

  <stairville-z120m-6ch>

  (fixture-attributes
    (attr-continuous intensity '(0 100) 0)
    (attr-colour colour white)
    (attr-continuous strobe-frequency '(1 25) 1)
    (attr-list strobe '(off on random) 'off)
    (attr-continuous white-weirdness '(0 100) 0))

  (let ((intensity (get-attr intensity))
        (rgbw (colour-as-rgbw-weirdness (get-attr colour)
                                        (/ (get-attr white-weirdness) 100))))
    (set-chan8 1 (percent->dmxval8 intensity))
    (set-chan8 3 (percent->dmxval8 (car rgbw)))
    (set-chan8 4 (percent->dmxval8 (cadr rgbw)))
    (set-chan8 5 (percent->dmxval8 (caddr rgbw)))
    (set-chan8 6 (percent->dmxval8 (cadddr rgbw))))
  (cond
    ((eq? (get-attr strobe) 'on)
     (set-chan8 2 (scale-and-clamp-to-range
                    (get-attr 'strobe-frequency)
                    '(1 25)
                    '(106 165))))
    ((eq? (get-attr strobe) 'random)
     (set-chan8 2 (scale-and-clamp-to-range
                    (get-attr strobe-frequency)
                    '(1 25)
                    '(181 240))))
    (else (set-chan8 2 255))))
