;;
;; starlet/crossfade.scm
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
(define-module (starlet crossfade)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 exceptions)
  #:use-module (starlet playback)
  #:use-module (starlet clock)
  #:use-module (starlet cue-list)
  #:use-module (starlet colours)
  #:use-module (starlet fixture)
  #:use-module (starlet state)
  #:use-module (starlet transition-effect)
  #:export (crossfade))


(define-record-type <fade-times>
  (make-fade-times up-time
                   down-time
                   attr-time
                   up-delay
                   down-delay
                   attr-delay)
  fade-times?
  (up-time      get-fade-up-time)
  (down-time    get-fade-down-time)
  (attr-time    get-fade-attr-time)
  (up-delay     get-fade-up-delay)
  (down-delay   get-fade-down-delay)
  (attr-delay   get-fade-attr-delay))


(define (snap-fade start-val
                   target-val
                   clock)
  (if (> (elapsed-fraction clock) 0)
      target-val
      start-val))


(define (colour-fade start-val
                     end-val
                     clock)

  (unless (and (colour? start-val)
               (colour? end-val))
    (raise-exception (make-exception
                       (make-exception-with-message
                         "Non-colour arguments given to colour-fade")
                       (make-exception-with-irritants
                         (list start-val end-val)))))

  (interpolate-colour start-val
                      end-val
                      (elapsed-fraction clock)
                      #:interpolation-type 'linear-cmy))


(define (simple-fade start-val
                     end-val
                     clock)

  (unless (and (number? start-val)
               (number? end-val))
    (raise-exception (make-exception
                       (make-exception-with-message
                         "Non-number arguments given to simple-fade")
                       (make-exception-with-irritants
                         (list start-val end-val)))))

  (+ start-val
     (* (- end-val start-val)
        (elapsed-fraction clock))))


(define (replace-noval val replacement)
  (if (eq? 'no-value val) replacement val))


(define (make-intensity-fade prev-val
                             target-val-in
                             up-clock
                             down-clock)
  (let ((target-val (replace-noval target-val-in 0.0)))

    (cond

      ;; Number to number, fading up
      ((and (number? target-val)
            (number? prev-val)
            (> target-val prev-val))
       (lambda ()
         (simple-fade prev-val
                      target-val
                      up-clock)))

      ;; Number to number, fading down
      ((and (number? target-val)
            (number? prev-val)
            (< target-val prev-val))
       (lambda ()
         (simple-fade prev-val
                      target-val
                      down-clock)))

      ;; Number to number, staying the same
      ;; NB We still need a static value so that fade-start-val can "unwrap" it
      ((and (number? target-val)
            (number? prev-val))
       (lambda () prev-val))

      ;; Everything else, e.g. number to effect
      (else
        (lambda ()
          (max
            (simple-fade (value->number prev-val)
                         0
                         down-clock)
            (simple-fade 0
                         (value->number target-val)
                         up-clock)))))))


(define (make-list-attr-fade start-val
                             target-val
                             clock)
  (lambda ()
    (snap-fade start-val
               target-val
               clock)))


(define (make-general-fade fade-func
                           start-val
                           target-val
                           clock)

  (if (and (not (procedure? target-val))
           (not (eq? target-val 'no-value))
           (not (eq? start-val 'no-value)))

    ;; It makes sense to do a fade
    (let ((real-start-val (value->number start-val)))
      (lambda ()
        (fade-func real-start-val
                   target-val
                   clock)))

    ;; A fade doesn't make sense, so make do with a snap transition
    (lambda ()
      (snap-fade start-val
                 target-val
                 clock))))


(define (fade-start-val pb fix attr)
  (let ((val-in-pb (state-find fix attr pb)))
    (if (eq? val-in-pb 'no-value)

        ;; Not currently in playback - fade from home value
        (get-attr-home-val fix attr)

        ;; Currently in playback - fade from current value
        ;; by running the outer crossfade function
        (val-in-pb))))


(define (dark? a)
  (or (eq? a 'no-value)
      (and (number? a)
           (< a 1))))


(define (fix-attr-eq fa1 fa2)
  (and (eq? (car fa1) (car fa2))
       (eq? (cdr fa1) (cdr fa2))))


(define (fix-attrs-in-state state)
  (state-map (lambda (fix attr val) (cons fix attr))
             state))


(define (add-fix-attrs-to-list state old-list)
  (lset-union fix-attr-eq
              old-list
              (fix-attrs-in-state state)))


(define (fix-attrs-involved . states)
  (fold add-fix-attrs-to-list '() states))


(define (blank-everything in-state down-clock)
  (let ((out-state (make-empty-state)))
    (state-for-each
      (lambda (fix attr val)
        (when (intensity? attr)
          (set-in-state! out-state
                         fix
                         attr
                         (lambda ()
                           (simple-fade (val)
                                        0.0
                                        down-clock)))))
      in-state)
    out-state))


(define (make-fade-for-attribute-type type)
  (cond
    ((eq? type 'continuous) (cut make-general-fade simple-fade <...>))
    ((eq? type 'list) make-list-attr-fade)
    ((eq? type 'colour) (cut make-general-fade colour-fade <...>))
    (else
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Unrecognised attribute type")
                         (make-exception-with-irritants type))))))


(define* (crossfade up-time
                    #:optional
                    inp-down-time
                    #:key
                    (attr-time 0)
                    (up-delay 0)
                    (down-delay 0)
                    (attr-delay 0))
  (let ((down-time (if inp-down-time
                     inp-down-time
                     up-time)))
    (make-transition
      (incoming-state current-state clock)
      (let ((up-clock (make-delayed-clock clock up-delay up-time))
            (down-clock (make-delayed-clock clock down-delay down-time))
            (attribute-clock (make-delayed-clock clock attr-delay attr-time)))
        (let ((overlay-state (blank-everything current-state down-clock)))
          (state-for-each
            (lambda (fixture attr target-val)

              (let ((start-val (fade-start-val current-state fixture attr)))

                (if (intensity? attr)

                  ;; Intensity attribute
                  (set-in-state! overlay-state fixture attr
                                 (make-intensity-fade start-val
                                                      target-val
                                                      up-clock
                                                      down-clock))

                  ;; Non-intensity attribute
                  (let ((attribute-obj (find-attr fixture attr)))

                    (unless attribute-obj
                      (raise-exception (make-exception
                                         (make-exception-with-message
                                           "Attribute not found")
                                         (make-exception-with-irritants
                                           (list fixture attr)))))

                    (let* ((atype (get-attr-type attribute-obj))
                           (make-fade-func (make-fade-for-attribute-type atype)))

                      (set-in-state! overlay-state fixture attr
                                     (make-fade-func start-val
                                                     target-val
                                                     attribute-clock)))))))

            incoming-state)
          (values overlay-state
                  (max
                    (+ up-time up-delay)
                    (+ down-time down-delay)
                    (+ attr-time attr-delay))))))))
