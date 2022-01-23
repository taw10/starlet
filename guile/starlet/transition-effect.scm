;;
;; starlet/transition-effect.scm
;;
;; Copyright © 2021-2022 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet transition-effect)
  #:use-module (oop goops)
  #:export (<transition-effect>
             transition-effect?
             transition-func
             make-transition))


(define-class <transition-effect> (<object>)
  (func
    #:init-value #f
    #:init-keyword #:func
    #:getter transition-func))


(define (transition-effect? a)
  (is-a? a <transition-effect>))


(define-syntax make-transition
  (syntax-rules ()
    ((_ (a b) expr ...)
     (make <transition-effect>
           #:func (lambda (a b)
                    expr ...)))))
