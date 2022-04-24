;;
;; starlet/fixture-library/stairville/octagon-theater-cw-ww.scm
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
(define-module (starlet fixture-library stairville octagon-theater-cw-ww)
  #:use-module (starlet fixture)
  #:export (<stairville-octagon-theater-cw-ww>))

(define-fixture

  <stairville-octagon-theater-cw-ww>

  (list
    (attr-continuous 'intensity '(0 100) 0)
    (attr-continuous 'colour-temperature '(2800 6400) 4600))

  (get-attr set-chan8 set-chan16)

  (let ((coltemp (get-attr 'colour-temperature)))
    (set-chan8 1 (scale-and-clamp-to-range coltemp '(2800 6400) '(0 255)))
    (set-chan8 2 (scale-and-clamp-to-range coltemp '(2800 6400) '(255 0))))
  (set-chan8 3 0)  ;; Strobe
  (set-chan8 4 0)  ;; Mode (0-15 = direct control)
  (set-chan8 5 (percent->dmxval8 (get-attr 'intensity))))
