;;
;; starlet/colours.scm
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
(define-module (starlet colours)
  #:use-module (oop goops)
  #:use-module (ice-9 exceptions)
  #:export (<colour>
             colour?
             cmy
             rgb
             colour-as-cmy
             colour-as-rgb
             colour-as-rgbw

             cyan
             magenta
             yellow
             red
             green
             blue

             interpolate-colour
             white

             <colour-component-id>
             colour-component-id?
             colour-component-id
             get-colour-component
             extract-colour-component))


(define-class <colour> (<object>)
  (type
    #:init-form (error "Colour type must be specified")
    #:init-keyword #:type
    #:getter colour-type)

  (value
    #:init-form (error "Colour value must be specified")
    #:init-keyword #:value
    #:getter colour-value))


(define (colour? c)
  (is-a? c <colour>))


(define cyan car)
(define magenta cadr)
(define yellow caddr)

(define red car)
(define green cadr)
(define blue caddr)

(define-method (display (col <colour>) port)
  (format port "#<<colour> ~a ~a>"
          (colour-type col)
          (colour-value col)))

(define (three-sf n)
  (/ (round (* (exact->inexact n) 10)) 10))

(define-method (write (col <colour>) port)
  (let ((cmy (colour-as-cmy col)))
    (format port "(cmy ~a ~a ~a)"
            (three-sf (cyan cmy))
            (three-sf (magenta cmy))
            (three-sf (yellow cmy)))))


(define (cmy c m y)
  (make <colour>
        #:type 'cmy
        #:value (list c m y)))


(define (rgb r g b)
  (make <colour>
        #:type 'rgb
        #:value (list r g b)))


(define white
  (cmy 0 0 0))


(define (colour-as-rgb col)
  (let ((val (colour-value col)))
    (case (colour-type col)

      ((rgb)
       val)

      ((cmy)
       (list (- 100 (red val))
             (- 100 (green val))
             (- 100 (blue val))))

      (else
        (raise-exception (make-exception
                           (make-exception-with-message "Unrecognised colour type")
                           (make-exception-with-irritants (colour-type col))))))))


(define (colour-as-rgbw col)
  (let ((rgb (colour-as-rgb col)))
    (let ((w (apply min rgb)))
      (list (- (red rgb) w)
            (- (green rgb) w)
            (- (blue rgb) w)
            w))))


(define (colour-as-cmy col)
  (let ((val (colour-value col)))
    (case (colour-type col)

      ((cmy)
       val)

      ((rgb)
       (list (- 100 (red val))
             (- 100 (green val))
             (- 100 (blue val))))

      (else
        (raise-exception (make-exception
                           (make-exception-with-message "Unrecognised colour type")
                           (make-exception-with-irritants (colour-type col))))))))


(define (interpolate-cmy a b frac)
  (let ((cmy1 (colour-as-cmy a))
        (cmy2 (colour-as-cmy b)))
    (cmy
      (+ (cyan cmy1) (* frac (- (cyan cmy2) (cyan cmy1))))
      (+ (magenta cmy1) (* frac (- (magenta cmy2) (magenta cmy1))))
      (+ (yellow cmy1) (* frac (- (yellow cmy2) (yellow cmy1)))))))


(define* (interpolate-colour a b frac #:key (interpolation-type 'linear-cmy))
  (case interpolation-type

    ((linear-cmy)
     (interpolate-cmy a b frac))

    (else
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Unrecognised colour interpolation type")
                         (make-exception-with-irritants interpolation-type))))))


(define-class <colour-component-id> (<object>)
  (component
    #:init-form (error "Colour component must be specified")
    #:init-keyword #:component
    #:getter get-colour-component))


(define (colour-component-id? a)
  (is-a? a <colour-component-id>))


(define (colour-component-id a)
  (make <colour-component-id>
        #:component a))


(define (extract-colour-component col component-id)
  (cond
    ((eq? (get-colour-component component-id) 'cyan)
     (cyan (colour-as-cmy col)))
    ((eq? (get-colour-component component-id) 'magenta)
     (magenta (colour-as-cmy col)))
    ((eq? (get-colour-component component-id) 'yellow)
     (yellow (colour-as-cmy col)))
    ((eq? (get-colour-component component-id) 'red)
     (red (colour-as-rgb col)))
    ((eq? (get-colour-component component-id) 'green)
     (green (colour-as-rgb col)))
    ((eq? (get-colour-component component-id) 'blue)
     (blue (colour-as-rgb col)))
    (else (raise-exception (make-exception
                             (make-exception-with-message
                               "Invalid colour component ID")
                             (make-exception-with-irritants
                               (get-colour-component component-id)))))))
