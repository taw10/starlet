;;
;; starlet/fixture-library/robe/dl7s/mode1.scm
;;
;; Copyright © 2020-2023 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet fixture-library robe dl7s)
  #:use-module (oop goops)
  #:use-module (starlet fixture)
  #:use-module (starlet utils)
  #:use-module (starlet attributes)
  #:use-module (starlet colours)
  #:use-module (starlet scanout)
  #:export (<robe-dl7s-mode1>))


(define virtual-colour-wheel
  '((#f 0)
    (lee4-medium-bastard-amber 2)
    (lee10-medium-yellow 4)
    (lee19-fire 6)
    (lee26-bright-red 8)
    (lee58-lavender 10)
    (lee68-sky-blue 12)
    (lee71-tokyo-blue 14)
    (lee79-just-blue 16)
    (lee88-lime-green 18)
    (lee90-dark-yellow-green 20)
    (lee100-spring-yellow 22)
    (lee101-yellow 24)
    (lee102-light-amber 26)
    (lee103-straw 28)
    (lee104-deep-amber 30)
    (lee105-orange 32)
    (lee106-primary-red 34)
    (lee111-dark-pink 36)
    (lee115-peacock-blue 38)
    (lee116-medium-blue-green 40)
    (lee117-steel-blue 42)
    (lee118-light-blue 44)
    (lee119-dark-blue 46)
    (lee120-deep-blue 48)
    (lee121-lee-green 50)
    (lee128-bright-pink 52)
    (lee131-marine-blue 54)
    (lee132-medium-blue 56)
    (lee134-golden-amber 58)
    (lee135-deep-golden-amber 60)
    (lee136-pale-lavender 62)
    (lee137-special-lavender 64)
    (lee138-pale-green 66)
    (lee139-primary-green 68)
    (lee141-bright-blue 70)
    (lee147-apricot 72)
    (lee148-bright-rose 74)
    (lee152-pale-gold 76)
    (lee154-pale-rose 78)
    (lee157-pink 80)
    (lee158-deep-orange 82)
    (lee162-bastard-amber 84)
    (lee164-flame-red 86)
    (lee165-daylight-blue 88)
    (lee169-lilac-tint 90)
    (lee170-deep-lavender 92)
    (lee172-lagoon-blue 94)
    (lee179-chrome-orange 96)
    (lee180-dark-lavender 98)
    (lee181-congo-blue 100)
    (lee197-alice-blue 102)
    (lee201-full-ct-blue 104)
    (lee202-half-ct-blue 106)
    (lee203-quarter-ct-blue 108)
    (lee204-full-ct-orange 110)
    (lee205-half-ct-orange 112)
    (lee206-quarter-ct-orange 114)
    (lee247-lee-minus-green 116)
    (lee247-half-minus-green 118)
    (lee281-threequarter-ct-blue 120)
    (lee285-threequarter-ct-orange 122)
    (lee352-glacier-blue 124)
    (lee353-lighter-blue 126)
    (lee715-cabana-blue 128)
    (lee778-millennium-gold 130)
    (lee793-vanity-fair 132)
    (deep-red 193)
    (deep-blue 195)
    (orange 197)
    (green 199)
    (magenta 201)
    (congo-blue 203)
    (pink 205)
    (lavender 207)
    (laser-green 209)
    (ctb 211)
    (minus-green 213)
    (minus-half-green 215)))

(define static-gobo-wheel
  '((#f 0)
    (water 7)
    (rugged-isles 14)
    (quadrangle-screen 21)
    (whirl 28)
    (breakup 36)
    (blur-breakup 43)
    (knitting 50)
    (grit 57)))

(define rotating-gobo-wheel
  '((#f 0)
    (rose 7)
    (water-line 11)
    (tree-trunk 15)
    (high-window 20)
    (grid 24)
    (clouds 29)))


;; FIXME: Gobo shaking (both wheels)
;; FIXME: Rainbow effect on colour wheel (???)
;; FIXME: Fine control iris, zoom
(define-fixture

  <robe-dl7s-mode1>

  (fixture-attributes
    (attr-continuous intensity '(0 100) 0)
    (attr-continuous pan '(0 540) 270)
    (attr-continuous tilt '(0 270) 135)
    (attr-list strobe '(#f on random) #f)
    (attr-continuous strobe-frequency '(0 25) 25 "Frequencies not calibrated")
    (attr-list prism '(#t #f) #f)
    (attr-colour colour white)
    (attr-list colwheel (map car virtual-colour-wheel) #f "Has priority over 'colour' attribute")
    (attr-continuous colour-temperature '(2700 8000) 3200)
    (attr-list animation-wheel '(#t #f) #f)
    (attr-continuous animation-wheel-position '(-100 100) 0)
    (attr-continuous animation-wheel-speed '(-100 100) 0)
    (attr-list gobo (map car static-gobo-wheel) #f)
    (attr-continuous gobo-shift '(0 100) 0)
    (attr-list rotating-gobo (map car rotating-gobo-wheel) #f)
    (attr-continuous rotating-gobo-speed '(-100 100) 0)
    (attr-continuous prism-rotation-speed '(-100 100) 0)
    (attr-continuous frost '(0 100) 0)
    (attr-continuous zoom '(0 100) 50)
    (attr-continuous iris '(0 100) 0)
    (attr-continuous barndoor-all-rotation '(-45 45) 0)
    (attr-continuous barndoor1 '(0 100) 0)
    (attr-continuous barndoor2 '(0 100) 0)
    (attr-continuous barndoor3 '(0 100) 0)
    (attr-continuous barndoor4 '(0 100) 0)
    (attr-continuous barndoor1-rotation '(-25 25) 0)
    (attr-continuous barndoor2-rotation '(-25 25) 0)
    (attr-continuous barndoor3-rotation '(-25 25) 0)
    (attr-continuous barndoor4-rotation '(-25 25) 0)
    (attr-continuous focus '(0 100) 50))

  (set-chan16 1 (scale-to-range (get-attr pan) '(0 540) '(0 65535)))
  (set-chan16 3 (scale-to-range (get-attr tilt) '(0 270) '(0 65535)))

  (set-chan8 5 0) ;; Pan/tilt speed/time: maximum speed
  (set-chan8 6 0) ;; Power/special function: default
  (set-chan8 7 0) ;; Colour mode: default

  (set-chan8 8 (lookup (get-attr colwheel) virtual-colour-wheel))

  (let ((cmy (colour-as-cmy (get-attr colour))))
    (set-chan16 9 (percent->dmxval16 (cyan cmy)))
    (set-chan16 11 (percent->dmxval16 (magenta cmy)))
    (set-chan16 13 (percent->dmxval16 (yellow cmy))))

  (set-chan8 15
             (scale-and-clamp-to-range (get-attr colour-temperature)
                                       '(8000 2700) '(0 255)))

  (set-chan8 16 0)   ;; Green correction: uncorrected white
  (set-chan8 17 0)   ;; Colour mix control: virtual colour wheel has priority
  (set-chan8 18 0)   ;; Rotating gobo selection speed: maximum
  (set-chan8 19 0)   ;; Everything time: off (???)

  (let ((ani-active (get-attr animation-wheel)))
    (set-chan8 20 (if ani-active
                    0
                    (scale-to-range (get-attr animation-wheel-position)
                                    '(-100 100) '(19 127)))) ;; 73 = stop
    (set-chan8 21 (scale-to-range (get-attr animation-wheel-speed)
                                  '(100 -100) '(1 255)))) ;; 128 = stop
  (set-chan8 22 0)   ;; Animation wheel macro: no function

  (set-chan8 23 (lookup (get-attr gobo) static-gobo-wheel))
  (set-chan8 24 (percent->dmxval8 (get-attr gobo-shift)))

  (set-chan8 25 (lookup (get-attr rotating-gobo) rotating-gobo-wheel))
  (set-chan8 26 (scale-to-range (get-attr rotating-gobo-speed)
                                '(-100 100) '(1 255)))  ;; 128 = stop
  (set-chan8 27 0)  ;; Rotating gobo fine adjustment (default)

  (set-chan8 28 (if (get-attr prism) 50 0))
  (set-chan8 29 (scale-to-range (get-attr prism-rotation-speed)
                                '(100 -100) '(1 255)))  ;; 128 = stop, <128=forwards
  (set-chan8 30 (scale-to-range (get-attr frost) '(0 100) '(0 180)))
  (set-chan8 31 (scale-to-range (get-attr iris) '(0 100) '(0 180)))
  (set-chan16 33 (percent->dmxval16 (get-attr zoom)))
  (set-chan16 35 (percent->dmxval16 (get-attr focus)))

  (set-chan8 38 (scale-to-range (get-attr barndoor-all-rotation) '(-45 45) '(0 255)))
  (set-chan8 39 (percent->dmxval8 (get-attr barndoor1)))
  (set-chan8 40 (scale-to-range (get-attr barndoor1-rotation) '(-25 25) '(0 255)))
  (set-chan8 41 (percent->dmxval8 (get-attr barndoor2)))
  (set-chan8 42 (scale-to-range (get-attr barndoor2-rotation) '(-25 25) '(0 255)))
  (set-chan8 43 (percent->dmxval8 (get-attr barndoor3)))
  (set-chan8 44 (scale-to-range (get-attr barndoor3-rotation) '(-25 25) '(0 255)))
  (set-chan8 45 (percent->dmxval8 (get-attr barndoor4)))
  (set-chan8 46 (scale-to-range (get-attr barndoor4-rotation) '(-25 25) '(0 255)))
  (set-chan8 47 0)  ;; Framing shutter macro: no function
  (set-chan8 48 128)  ;; Framing shutter macro speed: default

  (let ((strb (get-attr strobe)))
    (set-chan8 49
               (cond
                 ((not strb) 32)
                 ((eq? strb 'on)
                  (scale-to-range (get-attr strobe-frequency) '(1 25) '(64 95)))
                 ((eq? strb 'random)
                  (scale-to-range (get-attr strobe-frequency) '(1 25) '(192 223))))))

  (set-chan16 50 (percent->dmxval16 (get-attr intensity))))
