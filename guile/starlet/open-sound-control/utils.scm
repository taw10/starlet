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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 receive)
  #:export (osc-playback-controls
            osc-select-button
            osc-parameter-encoder
            osc-smart-potentiometer
            osc-state-fader
            send-selection-updates-to))


(define* (osc-playback-controls pb server addr go-button stop-button back-button
                                #:key (min-time-between-presses 0.2))

  (let ((time-last-press 0))
    (add-osc-method
      server
      (string-append go-button "/press")
      ""
      (lambda ()
        (let ((time-this-press (hirestime)))
          (if (> time-this-press (+ time-last-press min-time-between-presses))
            (go! pb)
            (display "Too soon after last press!\n"))
          (set! time-last-press time-this-press)))))

  (add-osc-method server (string-append stop-button "/press") "" (lambda () (stop! pb)))
  (add-osc-method server (string-append back-button "/press") "" (lambda () (back! pb)))

  ;; LEDs
  (osc-send addr (string-append back-button "/set-led") 'green)

  (add-and-run-hook!
    (state-change-hook pb)
    (lambda (new-state)

      (if (eq? new-state 'running)
        (osc-send addr (string-append stop-button "/set-led") 'green)
        (osc-send addr (string-append stop-button "/set-led") 'off))

      (cond
        ((eq? new-state 'pause)
         (osc-send addr (string-append go-button "/set-led") 'orange))
        ((eq? new-state 'ready)
         (osc-send addr (string-append go-button  "/set-led") 'green))
        ((eq? new-state 'running)
         (osc-send addr (string-append go-button  "/set-led") 'green))
        (else
          (osc-send addr (string-append go-button  "/set-led") 'off))))

    (playback-state pb)))


(define (osc-select-button fix server addr button)

  (add-osc-method
    server
    (string-append button "/press")
    ""
    (lambda ()
      (toggle-sel fix)))

  (add-and-run-hook!
    selection-hook
    (lambda (sel)
      (if (selected? fix)
        (osc-send addr (string-append button "/set-led") 'orange)
        (osc-send addr (string-append button "/set-led") 'red)))
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


(define (osc-parameter-encoder attr server addr encoder)

  (add-osc-method server (string-append encoder "/inc") ""
                  (lambda () (encoder-inc attr 3)))

  (add-osc-method server (string-append encoder "/dec") ""
                  (lambda () (encoder-inc attr -3)))

  (add-osc-method server (string-append encoder "/inc-fine") ""
                  (lambda () (encoder-inc attr 1)))

  (add-osc-method server (string-append encoder "/dec-fine") ""
                  (lambda () (encoder-inc attr -1)))

  (add-and-run-hook!
    selection-hook
    (lambda (sel)
      (if (any
            (lambda (fix)
              (fixture-has-attr? fix attr))
            (get-selection))
        (osc-send addr (string-append encoder "/set-led") 'green)
        (osc-send addr (string-append encoder "/set-led") 'off)))
    (get-selection)))


(define (ccval->percent n)
  (/ (* n 100) 127))


(define (osc-state-fader server addr fader state)
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

    (osc-send addr (string-append fader "/enable"))
    (osc-send addr (string-append fader "/set-pickup") 0)
    (add-osc-method server (string-append fader "/value-change") "i"
                    (lambda (v) (set! fader-val v)))))


(define (send-selection-updates-to addr)
  (add-hook!
    selection-hook
    (lambda (sel)
      (osc-send
        addr
        "/starlet/selection/update"
        (get-selection-as-string)))))


(define (fader-up-gradients initial-vals
                            max-vals
                            congruent-val)
  (map (lambda (initial-val attr-max)
         (if (< congruent-val 127)
               (/ (- attr-max initial-val)
                  (- 127 congruent-val))
               0))
       initial-vals
       max-vals))


(define (fader-down-gradients initial-vals
                              min-vals
                              congruent-val)
  (map (lambda (initial-val attr-min)
         (if (> congruent-val 0)
               (/ (- initial-val attr-min)
                  congruent-val)
               0))
       initial-vals
       min-vals))


(define (fixtures-with-attr fixture-list attr-name)
  (let ((fix-attrs
          (map (lambda (fix)
                 (let ((attr (find-attr fix attr-name)))
                   (if attr
                     (cons fix attr)
                     (cons #f #f))))
               fixture-list)))
    (values
      (filter (lambda (x) x) (map car fix-attrs))
      (filter (lambda (x) x) (map cdr fix-attrs)))))


(define (current-values fixture-list attr-name)
  (map (lambda (fix)
         (current-value fix attr-name))
       fixture-list))


(define-record-type <smart-potentiometer>
  (smart-pot-record addr
                    pot-method
                    initial-vals
                    min-vals
                    max-vals
                    congruent-val
                    up-gradients
                    dn-gradients)
  smart-pot?
  (addr          get-target-addr)
  (pot-method    get-method)
  (initial-vals  get-initial-vals    set-initial-vals)
  (min-vals      get-min-vals        set-min-vals)
  (max-vals      get-max-vals        set-max-vals)
  (congruent-val get-congruent-val   set-congruent-val)
  (up-gradients  get-up-gradients    set-up-gradients)
  (dn-gradients  get-dn-gradients    set-dn-gradients))


(define (make-smart-potentiometer server addr pot-method callback)

  (let ((sp (smart-pot-record addr pot-method '() '() '() 0 '() '())))

    (add-osc-method
      server
      (string-append pot-method "/value-change")
      "i"
      (lambda (new-cc-value)
        (callback
          (map
            (lambda (initial-val gradient)
              (+ initial-val
                 (* gradient
                    (- new-cc-value (get-congruent-val sp)))))
            (get-initial-vals sp)
            (if (> new-cc-value (get-congruent-val sp))
              (get-up-gradients sp)
              (get-dn-gradients sp))))))

    sp))


(define (reset-gradients sp)
  (unless (nil? (get-initial-vals sp))
    (set-congruent-val sp
                       (mean
                         (map
                           (lambda (val min-val max-val)
                             (scale-to-range val (list min-val max-val) '(0 127)))
                           (get-initial-vals sp)
                           (get-min-vals sp)
                           (get-max-vals sp))))
    (set-up-gradients sp
                      (fader-up-gradients
                        (get-initial-vals sp)
                        (get-max-vals sp)
                        (get-congruent-val sp)))
    (set-dn-gradients sp
                      (fader-up-gradients
                        (get-initial-vals sp)
                        (get-min-vals sp)
                        (get-congruent-val sp)))
    (osc-send
      (get-target-addr sp)
      (string-append (get-method sp) "/set-pickup")
      (get-congruent-val sp))))


(define (osc-smart-potentiometer attr-name
                                 server
                                 addr
                                 potentiometer)

  (let ((fixtures '()))

    ;; First, create a smart potentiometer object and tell it to
    ;; set the attribute values in the programmer state
    (let ((smart-pot
            (make-smart-potentiometer
              server
              addr
              potentiometer
              (lambda (new-vals)
                (for-each
                  (lambda (fix new-val)
                    (set-in-state! programmer-state
                                   fix
                                   attr-name
                                   new-val
                                   potentiometer))
                  fixtures new-vals)))))

      ;; Next, set up a selection hook callback to update the list of
      ;; fixtures we are controlling
      (add-and-run-hook!
        selection-hook
        (lambda (selection)
          (receive
            (new-fixtures attrs)
            (fixtures-with-attr selection attr-name)
            (if (nil? new-fixtures)
              (osc-send addr (string-append potentiometer "/disable"))
              (begin
                (set! fixtures new-fixtures)
                (let ((ranges (map get-attr-range attrs)))
                  (set-min-vals smart-pot (map first ranges))
                  (set-max-vals smart-pot (map second ranges)))
                (set-initial-vals smart-pot (current-values fixtures attr-name))
                (reset-gradients smart-pot)
                (osc-send addr (string-append potentiometer "/enable"))))))
        (get-selection))

      ;; Finally, arrange for the smart potentiometer object to be notified
      ;; if the values change externally
      (add-update-hook!
        programmer-state
        (lambda (source)
          (unless (eq? source potentiometer)
            (set-initial-vals smart-pot (current-values fixtures attr-name))
            (reset-gradients smart-pot)))))))
