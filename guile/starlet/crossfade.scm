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
  #:use-module (starlet playback)
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


(define (match-fix-attr attr-el fix attr)
  (cond

   ((fixture? attr-el)
    (eq? attr-el fix))

   ((and (pair? attr-el)
         (fixture? (car attr-el))
         (symbol? (cdr attr-el)))
    (and (eq? (car attr-el) fix)
         (eq? (cdr attr-el) attr)))

   ((list? attr-el)
    (and (memq fix attr-el)
         (memq attr attr-el)))

   (else #f)))


(define (in-cue-part? cue-part fix attr)
  (find (lambda (p) (match-fix-attr p fix attr))
        (get-cue-part-attr-list cue-part)))


(define (cue-part-fade-times the-cue fix attr)

  (let ((the-cue-part
         (find (lambda (p) (in-cue-part? p fix attr))
               (get-cue-parts the-cue))))

    (if (cue-part? the-cue-part)
      (get-cue-part-fade-times the-cue-part)
      (get-cue-fade-times the-cue))))


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


(define (longest-fade-time fade-times)
  (max
    (+ (get-fade-down-time fade-times)
       (get-fade-down-delay fade-times))
    (+ (get-fade-up-time fade-times)
       (get-fade-up-delay fade-times))
    (+ (get-fade-attr-time fade-times)
       (get-fade-attr-delay fade-times))))


;; Work out how long it will take before we can forget about this cue
(define (cue-total-time the-cue)
  (let ((fade-times (cons (get-cue-fade-times the-cue)
                          (map get-cue-part-fade-times
                               (get-cue-parts the-cue)))))
    (fold max
          0
          (map longest-fade-time fade-times))))


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


(define (run-cue-index! pb cue-index)
  (let* ((the-cue (vector-ref (get-playback-cue-list pb) cue-index))
         (this-cue-state (get-tracked-state the-cue))
         (overlay-state (make-empty-state))
         (cue-clock (get-cue-clock the-cue)))

    (for-each
      (lambda (fix-attr)

        (let* ((fix (car fix-attr))
               (attr (cdr fix-attr))
               (fade-times (cue-part-fade-times the-cue fix attr))

               ;; The values for fading
               (start-val (fade-start-val pb fix attr))
               (target-val (state-find fix attr this-cue-state))
               ;; The clocks for things in this cue part
               (up-clock (make-delayed-clock cue-clock
                                             (get-fade-up-delay fade-times)
                                             (get-fade-up-time fade-times)))

               (down-clock (make-delayed-clock cue-clock
                                               (get-fade-down-delay fade-times)
                                               (get-fade-down-time fade-times)))

               (attribute-clock (make-delayed-clock cue-clock
                                                    (get-fade-attr-delay fade-times)
                                                    (get-fade-attr-time fade-times))))

          (if (intensity? attr)

              ;; Intensity attribute
              (set-in-state! overlay-state fix attr
                             (make-intensity-fade start-val
                                                  target-val
                                                  up-clock
                                                  down-clock))

              ;; Non-intensity attribute
              (let ((attribute-obj (find-attr fix attr)))

                (unless attribute-obj
                  (raise-exception (make-exception
                                     (make-exception-with-message
                                       "Attribute not found")
                                     (make-exception-with-irritants
                                       (list fix attr)))))

                (let* ((atype (get-attr-type attribute-obj))
                       (make-fade-func (make-fade-for-attribute-type atype)))

                  (set-in-state! overlay-state fix attr
                                 (make-fade-func start-val
                                                 target-val
                                                 attribute-clock)))))))

      (fix-attrs-involved pb this-cue-state))

    (atomically-overlay-state! pb overlay-state)
    (set-pb-cue-clock! pb cue-clock)
    (set-running-cue! pb the-cue)
    (reset-clock! cue-clock)
    (start-clock! cue-clock)
    (set-playback-state! pb 'running)))

