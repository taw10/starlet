;;
;; starlet/attributes.scm
;;
;; Copyright © 2022-2023 Thomas White <taw@bitwiz.org.uk>
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
             canonical-name
             friendly))


(define-class <starlet-attribute> (<object>)
  (canonical-name
    #:init-keyword #:name
    #:getter canonical-name)
  (friendly
    #:init-keyword #:friendly
    #:getter friendly))

(define (make-attribute canonical-name friendly)
  (make <starlet-attribute>
        #:name canonical-name
        #:friendly friendly))

(define (attribute? a)
  (is-a? a <starlet-attribute>))

(define-method (write (attribute <starlet-attribute>) port)
  (write
    (canonical-name attribute)
    port))

(define-method (canonical-name whatever)
  whatever)


(define-syntax define-attribute
  (syntax-rules ()
    ((_ name friendly-name)
     (define-public name (make-attribute (quote name) friendly-name)))))


;; The standard attribute names
(define-attribute intensity "Intensity (percentage of brightest)")
(define-attribute colour "Colour (colour object)")
(define-attribute colour-temperature "Colour temperature (K)")
(define-attribute strobe "Strobe active (boolean)")
(define-attribute strobe-frequency "Strobe rate (Hz)")
(define-attribute pan "Moving head pan angle (degrees +/- from home)")
(define-attribute tilt "Moving head tilt angle (degrees +/- from home)")
(define-attribute prism "Prism active (boolean)")
(define-attribute prism-rotation-speed "Prism rotation speed (+/- percentage of fastest, clockwise)")
(define-attribute frost "Frost active (percentage of maximum frost)")
(define-attribute hotspot "Hot spot (percentage of maximum peakiness)")
(define-attribute iris "Iris (percentage of maximum tightness (perhaps completely closed)")
(define-attribute zoom "Zoom (percentage of tightest zoom)")
(define-attribute focus "Focus (percentage of nearest focus)")
(define-attribute barndoor-all-rotation "Rotation of all barndoors together (degrees +/- from home)")
(define-attribute barndoor1 "Barndoor 1 position (percentage of fully in position)")
(define-attribute barndoor2 "Barndoor 2 position (percentage of fully in position)")
(define-attribute barndoor3 "Barndoor 3 position (percentage of fully in position)")
(define-attribute barndoor4 "Barndoor 4 position (percentage of fully in position)")
(define-attribute barndoor1-rotation "Barndoor 1 rotation (degrees +/- from home)")
(define-attribute barndoor2-rotation "Barndoor 2 rotation (degrees +/- from home)")
(define-attribute barndoor3-rotation "Barndoor 3 rotation (degrees +/- from home)")
(define-attribute barndoor4-rotation "Barndoor 4 rotation (degrees +/- from home)")
(define-attribute beamtype "Beam type")
(define-attribute colwheel "Colour wheel selection (#f or gel name)")
(define-attribute gobo "Gobo selection (#f or gobo name)")
(define-attribute gobo-shift "Fine position of gobo (percentage of maximum shift)")
(define-attribute animation-wheel "Animation wheel active (boolean)")
(define-attribute animation-wheel-position "Animation wheel position (-100 to 100, 0=central)")
(define-attribute animation-wheel-speed "Animation wheel rotation speed and direction (+/- percentage of fastest, clockwise)")
(define-attribute rotating-gobo "Rotating gobo selection (#f or gobo name)")
(define-attribute rotating-gobo-speed "Gobo rotation speed (+/- percentage of maximum speed, clockwise)")
(define-attribute white-weirdness "Weirdness of white (percentage of maximum weirdness)")

;; Duplicate names for convenience...
(define-public color colour)
(define-public color-temperature colour-temperature)


(define (intensity? a)
  (eq? intensity a))

