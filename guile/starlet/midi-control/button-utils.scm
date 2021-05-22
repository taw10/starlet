;;
;; starlet/midi-control/button-utils.scm
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
(define-module (starlet midi-control button-utils)
  #:use-module (starlet midi-control base)
  #:use-module (starlet state)
  #:use-module (starlet playback)
  #:export (make-go-button
            make-stop-button
            make-back-button
            select-on-button))


(define* (make-go-button pb button
                         #:key (channel #f))
    (register-midi-note-callback!
     #:channel channel
     #:note-number button
     #:func (lambda () (go! pb))))


(define* (make-stop-button pb button
                           #:key (channel #f))
    (register-midi-note-callback!
     #:channel channel
     #:note-number button
     #:func (lambda () (stop! pb))))


(define* (make-back-button pb button
                           #:key (channel #f))
    (register-midi-note-callback!
     #:channel channel
     #:note-number button
     #:func (lambda () (back! pb))))


(define* (select-on-button button fixture
                           #:key (channel #f))
  (register-midi-note-callback!
   #:channel channel
   #:note-number button
   #:func (lambda () (sel fixture))))
