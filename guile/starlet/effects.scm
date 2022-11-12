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
  #:use-module (starlet state)
  #:use-module (starlet attributes)
  #:export (flash
             sinewave
             flash-chase))


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


(define (hump t on-time)
  (cond
    ((< t 0.0) 0.0)
    ((> t on-time) 0.0)
    (else (* 100 (sin (* pi (/ t on-time)))))))


(define* (flash-chase group
                      #:key (repeat-time 2) (offset-time 0.3) (on-time 0.5))
  (let ((clock (make-clock)))
    (for-each
      (lambda (fix idx)
        (at fix intensity
            (lambda ()
              (hump (- (euclidean-remainder (elapsed-time clock)
                                            repeat-time)
                       (* idx offset-time))
                    on-time))))
      group
      (iota (length group)))))
