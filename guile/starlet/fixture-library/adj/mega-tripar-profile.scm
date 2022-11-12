;;
;; starlet/fixture-library/adj/mega-tripar-profile.scm
;;
;; Copyright © 2020-2022 Thomas White <taw@bitwiz.me.uk>
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
(define-module (starlet fixture-library adj mega-tripar-profile)
  #:use-module (starlet scanout)
  #:use-module (starlet fixture)
  #:use-module (starlet attributes)
  #:use-module (starlet utils)
  #:use-module (starlet colours)
  #:export (<adj-mega-tripar-profile-3ch>
             <adj-mega-tripar-profile-4ch>))


;; 3 channel mode (RGB direct control)

(define-fixture

  <adj-mega-tripar-profile-3ch>

  (fixture-attributes
    (attr-continuous intensity '(0 100) 0)
    (attr-colour colour white))

  (let ((intensity (/ (get-attr intensity) 100))
        (rgb (colour-as-rgb (get-attr colour))))
    (set-chan8 1 (percent->dmxval8 (* intensity (car rgb))))
    (set-chan8 2 (percent->dmxval8 (* intensity (cadr rgb))))
    (set-chan8 3 (percent->dmxval8 (* intensity (caddr rgb))))))


;; 4 channel mode (RGB + separate intensity)

(define-fixture

  <adj-mega-tripar-profile-4ch>

  (fixture-attributes
    (attr-continuous intensity '(0 100) 0)
    (attr-colour colour white))

  (let ((rgb (colour-as-rgb (get-attr colour))))
    (set-chan8 1 (percent->dmxval8 (get-attr intensity)))
    (set-chan8 2 (percent->dmxval8 (car rgb)))
    (set-chan8 3 (percent->dmxval8 (cadr rgb)))
    (set-chan8 4 (percent->dmxval8 (caddr rgb)))))

