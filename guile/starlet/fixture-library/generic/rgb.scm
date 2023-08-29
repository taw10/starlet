;;
;; starlet/fixture-library/generic/rgb.scm
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
(define-module (starlet fixture-library generic rgb)
  #:use-module (starlet scanout)
  #:use-module (starlet fixture)
  #:use-module (starlet attributes)
  #:use-module (starlet utils)
  #:use-module (starlet colours)
  #:export (<generic-rgb>
             <generic-rgbw>))


(define (colour-as-rgbw-weirdness col weirdness)
  (let ((rgb (colour-as-rgb col)))
    (let ((w (* (- 1 weirdness) (apply min rgb))))
      (list (- (red rgb) w)
            (- (green rgb) w)
            (- (blue rgb) w)
            w))))


(define-fixture

  <generic-rgb>

  (fixture-attributes
    (attr-continuous intensity '(0 100) 0)
    (attr-colour colour white))

  (let ((intensity (get-attr intensity))
        (rgb (colour-as-rgb (get-attr colour))))
    (set-chan8 1 (percent->dmxval8 (* intensity 0.01 (car rgb))))
    (set-chan8 2 (percent->dmxval8 (* intensity 0.01 (cadr rgb))))
    (set-chan8 3 (percent->dmxval8 (* intensity 0.01 (caddr rgb))))))


(define-fixture

  <generic-rgbw>

  (fixture-attributes
    (attr-continuous intensity '(0 100) 0)
    (attr-colour colour white)
    (attr-continuous white-weirdness '(0 100) 0))

  (let ((intensity (get-attr intensity))
        (rgbw (colour-as-rgbw-weirdness (get-attr colour)
                                        (/ (get-attr white-weirdness) 100))))
    (set-chan8 1 (percent->dmxval8 (* 0.01 intensity (car rgbw))))
    (set-chan8 2 (percent->dmxval8 (* 0.01 intensity (cadr rgbw))))
    (set-chan8 3 (percent->dmxval8 (* 0.01 intensity (caddr rgbw))))
    (set-chan8 4 (percent->dmxval8 (* 0.01 intensity (cadddr rgbw))))))
