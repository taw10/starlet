;;
;; starlet/fixture-library/generic/dimmer.scm
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
(define-module (starlet fixture-library generic dimmer)
  #:use-module (starlet fixture)
  #:export (<generic-dimmer>))

(define-fixture

  <generic-dimmer>

  (list
    (attr-continuous 'intensity '(0 100) 0))

  (get-attr set-chan8)

  (set-chan8 1 (percent->dmxval8 (get-attr 'intensity))))

