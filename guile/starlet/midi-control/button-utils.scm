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
  #:use-module (starlet utils)
  #:export (make-go-button
            make-stop-button
            make-back-button
            select-on-button))


(define* (make-go-button controller pb button
                         #:key
                         (ready-note #f)
                         (pause-note #f)
                         (min-time-between-presses 0.2))
  (let ((time-last-press 0))
    (register-midi-note-callback!
      controller
      #:note-number button
      #:func (lambda ()
               (let ((time-this-press (hirestime)))
                 (if (> time-this-press (+ time-last-press min-time-between-presses))
                   (go! pb)
                   (display "Too soon after last press!\n"))
                 (set! time-last-press time-this-press)))))

  (when (or ready-note pause-note)
    (let ((state-change-func
            (lambda (new-state)
              (cond
                ((eq? new-state 'pause)
                 (send-note-on controller pause-note))
                ((eq? new-state 'ready)
                 (send-note-on controller ready-note))
                ((eq? new-state 'running)
                 (send-note-on controller ready-note))
                (else
                  (send-note-off controller ready-note))))))
      (add-hook!
        (state-change-hook pb)
        state-change-func)
      (state-change-func (playback-state pb)))))


(define* (make-stop-button controller pb button
                           #:key
                           (ready-note #f))
  (register-midi-note-callback!
    controller
    #:note-number button
    #:func (lambda () (stop! pb)))

  (when ready-note
    (add-hook!
      (state-change-hook pb)
      (lambda (new-state)
        (if (eq? new-state 'running)
            (send-note-on controller ready-note)
            (send-note-off controller ready-note))))))


(define* (make-back-button controller pb button
                           #:key
                           (ready-note #f))
  (register-midi-note-callback!
    controller
    #:note-number button
    #:func (lambda () (back! pb)))

  (when ready-note
    (send-note-on controller ready-note)))


(define* (select-on-button controller button fixture
                           #:key
                           (ready-note #f))
  (register-midi-note-callback!
    controller
    #:note-number button
    #:func (lambda () (sel fixture)))

  (when ready-note
    (send-note-on controller ready-note)))
