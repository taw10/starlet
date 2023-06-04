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
  #:use-module (starlet attributes)
  #:use-module (starlet playback)
  #:use-module (starlet selection)
  #:use-module (starlet fixture)
  #:use-module (starlet engine)
  #:use-module (starlet state)
  #:use-module (starlet utils)
  #:use-module (open-sound-control client)
  #:use-module (open-sound-control server-thread)
  #:use-module (srfi srfi-1)
  #:export (osc-playback-indicators
            osc-playback-controls
            osc-select-button
            osc-parameter-encoder
            osc-state-fader
            send-selection-updates-to))


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


(define (osc-select-button fix server button-method addr led)

  (add-osc-method server button-method ""
                  (lambda () (toggle-sel fix)))

  (add-and-run-hook!
    selection-hook
    (lambda (sel)
      (if (selected? fix)
        (osc-send addr led 'orange)
        (osc-send addr led 'red)))
    (get-selection)))


(define (encoder-inc attr-id n)
  (for-each
    (lambda (fix)
      (let ((attr (find-attr fix attr-id))
            (cval (current-value fix attr-id)))
        (cond
          ((eq? 'continuous (get-attr-type attr))
           (at fix attr-id (+ cval n)))
          ((eq? 'list (get-attr-type attr))
           (if (> n 0)
             (at fix attr-id (next-attr-item attr cval))
             (at fix attr-id (prev-attr-item attr cval)))))))
    (get-selection)))


(define (osc-parameter-encoder attr server encoder-method addr led)

  (add-osc-method server (string-append encoder-method "/inc") ""
                  (lambda () (encoder-inc attr 3)))

  (add-osc-method server (string-append encoder-method "/dec") ""
                  (lambda () (encoder-inc attr -3)))

  (add-osc-method server (string-append encoder-method "/inc-fine") ""
                  (lambda () (encoder-inc attr 1)))

  (add-osc-method server (string-append encoder-method "/dec-fine") ""
                  (lambda () (encoder-inc attr -1)))

  (add-and-run-hook!
    selection-hook
    (lambda (sel)
      (if (any
            (lambda (fix)
              (fixture-has-attr? fix attr))
            (get-selection))
        (osc-send addr led 'green)
        (osc-send addr led 'off)))
    (get-selection)))


(define (ccval->percent n)
  (/ (* n 100) 127))


(define (osc-state-fader server fader state)
  (let ((fader-val 0))
    (register-state!
      (lighting-state
        (state-for-each
          (lambda (fix attr val)
            (at fix attr
                (lambda ()

                  (if (intensity? attr)

                    ;; Intensity parameters get scaled according to the fader
                    (* 0.01 val (ccval->percent fader-val))

                    ;; Non-intensity parameters just get set in our new state,
                    ;; but only if the fader is up!
                    (if (> fader-val 0)
                      val
                      'no-value)))))
          state)))

    (add-osc-method server fader "i"
                    (lambda (v) (set! fader-val v)))))


(define (send-selection-updates-to addr)
  (add-hook!
    selection-hook
    (lambda (sel)
      (osc-send
        addr
        "/starlet/selection/update"
        (get-selection-as-string)))))
