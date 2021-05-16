;;
;; starlet/effects.scm
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
(define-module (starlet effects)
  #:use-module (starlet clock)
  #:export (flash
             sinewave))


(define pi (* 2 (acos 0)))

(define (square-wave time hz)
  (if (> (sin (* 2 pi hz time))
         0)
      100
      0))

(define (flash hz)
  (let ((clock (make-clock)))
    (lambda ()
      (square-wave (elapsed-time clock)
                   hz))))


(define (sinewave hz range-min range-max)
  (let ((clock (make-clock)))
    (lambda ()
      (+ range-min
         (* (/ (- range-max range-min) 2)
            (+ 1 (sin (* 2 pi hz (elapsed-time clock)))))))))
