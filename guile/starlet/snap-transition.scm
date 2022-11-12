;;
;; starlet/snap-transition.scm
;;
;; Copyright © 2021 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet snap-transition)
  #:use-module (oop goops)
  #:use-module (starlet playback)
  #:use-module (starlet state)
  #:use-module (starlet fixture)
  #:use-module (starlet transition-effect)
  #:use-module (starlet attributes)
  #:export (snap))


(define (blank-everything in-state)
  (let ((out-state (make-empty-state)))
    (state-for-each
      (lambda (fix attr val)
        (if (intensity? attr)
          (set-in-state! out-state fix attr (lambda () 0.0))
          (set-in-state! out-state fix attr (lambda () 'no-value))))
      in-state)
    out-state))


(define (snap)
  (make-transition
    (incoming-state current-state clock)
    (let ((overlay-state (blank-everything current-state)))
      (state-for-each
        (lambda (fix attr val)
          (set-in-state! overlay-state
                         fix
                         attr
                         (lambda () val)))
        incoming-state)
      (values overlay-state 0))))
