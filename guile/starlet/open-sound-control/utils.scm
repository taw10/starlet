;;
;; starlet/open-sound-control/utils.scm
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
(define-module (starlet open-sound-control utils)
  #:use-module (starlet playback)
  #:use-module (starlet selection)
  #:use-module (starlet utils)
  #:use-module (open-sound-control client)
  #:use-module (open-sound-control server-thread)
  #:export (osc-playback-indicators
            osc-playback-controls
            osc-select-button))


(define* (osc-playback-controls pb server go-method stop-method back-method
                                #:key (min-time-between-presses 0.2))

  (let ((time-last-press 0))
    (add-osc-method server go-method ""
                    (lambda ()
                      (let ((time-this-press (hirestime)))
                        (if (> time-this-press (+ time-last-press min-time-between-presses))
                          (go! pb)
                          (display "Too soon after last press!\n"))
                        (set! time-last-press time-this-press)))))

  (add-osc-method server stop-method "" (lambda () (stop! pb)))
  (add-osc-method server back-method "" (lambda () (back! pb))))


(define (osc-playback-indicators pb addr go-led stop-led back-led)

  (add-and-run-hook!
    (state-change-hook pb)
    (lambda (new-state)

      (if (eq? new-state 'running)
        (osc-send addr stop-led 'green)
        (osc-send addr stop-led 'off))

      (cond
        ((eq? new-state 'pause)
         (osc-send addr go-led 'orange))
        ((eq? new-state 'ready)
         (osc-send addr go-led 'green))
        ((eq? new-state 'running)
         (osc-send addr go-led 'green))
        (else
          (osc-send addr go-led 'off))))

    (playback-state pb))

  (osc-send addr back-led 'green))


(define (osc-select-button server button-method addr led fix)

  (add-osc-method server button-method ""
                  (lambda ()
                    (if (selected? fix)
                      (desel fix)
                      (sel fix))))

  (add-and-run-hook!
    selection-hook
    (lambda (sel)
      (if (selected? fix)
        (osc-send addr led 'orange)
        (osc-send addr led 'red)))
    (get-selection)))
