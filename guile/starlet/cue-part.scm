;;
;; starlet/cue-part
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
(define-module (starlet cue-part)
  #:use-module (srfi srfi-9)
  #:export (cue-part
            <cue-part>
            get-cue-part-state
            get-cue-part-transition
            set-cue-part-state!))


(define-record-type <cue-part>
  (cue-part state transition)
  cue-part?
  (state        get-cue-part-state
                set-cue-part-state!)
  (transition   get-cue-part-transition))
