;;
;; starlet/attributes.scm
;;
;; Copyright © 2022 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet attributes)
  #:use-module (oop goops)
  #:export (<starlet-attribute>
             make-attribute
             attribute?
             intensity?
             canonical-name))


(define-class <starlet-attribute> (<object>)
  (canonical-name
    #:init-keyword #:name
    #:getter canonical-name))

(define (make-attribute canonical-name)
  (make <starlet-attribute>
        #:name canonical-name))

(define (attribute? a)
  (is-a? a <starlet-attribute>))

(define-method (write (attribute <starlet-attribute>) port)
  (write
    (canonical-name attribute)
    port))

(define-method (canonical-name whatever)
  whatever)


;; The standard attribute names
;; Note that this list says nothing about the interpretation of the values
;; - that's left to the individual fixture definitions.
(define-public intensity (make-attribute 'intensity))
(define-public colour (make-attribute 'colour))
(define-public colour-temperature (make-attribute 'colour-temperature))
(define-public strobe (make-attribute 'strobe))
(define-public strobe-frequency (make-attribute 'strobe-frequency))
(define-public pan (make-attribute 'pan))
(define-public tilt (make-attribute 'tilt))
(define-public prism (make-attribute 'prism))
(define-public frost (make-attribute 'frost))
(define-public hotspot (make-attribute 'hotspot))
(define-public iris (make-attribute 'iris))
(define-public zoom (make-attribute 'zoom))
(define-public barndoor-rotation (make-attribute 'barndoor-rotation))
(define-public barndoor1 (make-attribute 'barndoor1))
(define-public barndoor2 (make-attribute 'barndoor2))
(define-public barndoor3 (make-attribute 'barndoor3))
(define-public barndoor4 (make-attribute 'barndoor4))
(define-public beamtype (make-attribute 'beamtype))
(define-public colwheel (make-attribute 'colwheel))
(define-public gobo (make-attribute 'gobo))

;; Duplicate names for convenience...
(define-public color colour)
(define-public color-temperature colour-temperature)


(define (intensity? a)
  (eq? intensity a))

