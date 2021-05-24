;;
;; starlet/playback.scm
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
(define-module (starlet playback)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 atomic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (starlet fixture)
  #:use-module (starlet state)
  #:use-module (starlet scanout)
  #:use-module (starlet utils)
  #:use-module (starlet clock)
  #:use-module (starlet colours)
  #:export (make-playback
            cue
            cue-part
            cut-to-cue-number!
            get-playback-cue-number
            run-cue-number!
            go!
            stop!
            back!
            cue-list
            set-playback-cue-list!
            print-playback
            state-change-hook))


;; A "playback" is a state which knows how to run cues
;; from a cue list
(define-class <starlet-playback> (<starlet-state>)
  (cue-list
    #:init-keyword #:cue-list
    #:getter get-playback-cue-list
    #:setter set-playback-cue-list!)

  (next-cue-index
    #:init-value 0
    #:getter get-next-cue-index
    #:setter set-next-cue-index!)

  (running-cue-clock
    #:init-value #f
    #:getter get-cue-clock
    #:setter set-cue-clock!)

  (current-state
    #:init-form (make-atomic-box 'ready)
    #:getter state-box)

  (state-change-hook
    #:init-form (make-hook 1)
    #:getter state-change-hook))


(define-record-type <cue-part>
  (make-cue-part attr-list
                 fade-times)
  cue-part?
  (attr-list    get-cue-part-attr-list)
  (fade-times   get-cue-part-fade-times))


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


(define-record-type <cue>
  (make-cue number
            state
            tracked-state
            fade-times
            preset-time
            track-intensities
            cue-parts)
  cue?
  (number             get-cue-number)
  (state              get-cue-state)
  (tracked-state      get-tracked-state
                      set-tracked-state!)
  (fade-times         get-cue-fade-times)
  (preset-time        get-cue-preset-time)
  (track-intensities  track-intensities)
  (cue-parts          get-cue-parts))


(define (get-playback-cue-number pb)
  (cue-index-to-number (get-playback-cue-list pb)
                       (max 0 (- (get-next-cue-index pb) 1))))

(define (qnum a)
  (/ (inexact->exact (* a 1000)) 1000))


(define (make-playback cue-list)
  (let ((new-playback (make <starlet-playback>
                        #:cue-list cue-list)))
    (register-state! new-playback)
    new-playback))


(define (cue-index-to-number cue-list cue-index)
  (get-cue-number (vector-ref cue-list cue-index)))


(define (cue-number-to-index cue-list cue-number)
  (vector-index (lambda (a)
                  (eqv? (get-cue-number a)
                        cue-number))
                cue-list))


(define (cut-to-cue-index! pb cue-index)
  (let ((cue-list (get-playback-cue-list pb)))
    (clear-state! pb)
    (set-next-cue-index! pb (+ cue-index 1))
    (set-cue-clock! pb #f)
    (atomic-box-set! (state-box  pb) 'ready)
    (run-hook (state-change-hook pb) 'ready)
    (let ((cue-state (calculate-tracking cue-list cue-index)))
      (state-for-each
        (lambda (fix attr val)
          (set-in-state! pb fix attr (lambda () val)))
        cue-state))))


(define (cut-to-cue-number! pb cue-number)

  (let* ((cue-list (get-playback-cue-list pb))
         (cue-index (cue-number-to-index cue-list (qnum cue-number))))

    (unless cue-index
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Invalid cue number")
                         (make-exception-with-irritants
                           (list pb cue-number)))))

    (cut-to-cue-index! pb cue-index)

    *unspecified*))


(define (run-cue-number! pb cue-number)

  (let* ((cue-list (get-playback-cue-list pb))
         (cue-index (cue-number-to-index cue-list (qnum cue-number))))

    (unless cue-index
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Invalid cue number")
                         (make-exception-with-irritants
                           (list pb cue-number)))))

    (set-next-cue-index! pb (+ cue-index 1))
    (run-cue-index! pb cue-index)
    *unspecified*))


(define (go! pb)
  (let ((clock (get-cue-clock pb)))
    (if (and clock
             (clock-stopped? clock))

        ;; Restart paused cue
        (begin (start-clock! clock)
               (atomic-box-set! (state-box pb) 'running)
               (run-hook (state-change-hook pb) 'running))

        ;; Run next cue
        (let ((next-cue-index (get-next-cue-index pb)))
          (if (< next-cue-index (vector-length (get-playback-cue-list pb)))
              (begin
                (run-cue-index! pb next-cue-index)
                (set-next-cue-index! pb (+ next-cue-index 1))
                *unspecified*)
              'no-more-cues-in-list)))))


(define (stop! pb)
  (let ((clock (get-cue-clock pb)))
    (when (and clock
               (not (clock-expired? clock)))
      (stop-clock! (get-cue-clock pb))
      (atomic-box-set! (state-box pb) 'pause)
      (run-hook (state-change-hook pb) 'pause))))


(define (back! pb)
  (let ((prev-cue-index (- (get-next-cue-index pb) 2)))
    (if (>= prev-cue-index 0)
        (begin (cut-to-cue-index! pb prev-cue-index)
               (atomic-box-set! (state-box pb) 'ready)
               (run-hook (state-change-hook pb) 'ready))
        'already-at-cue-zero)))


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
    ((eq? type 'continuous) (partial-start make-general-fade simple-fade))
    ((eq? type 'list) make-list-attr-fade)
    ((eq? type 'colour) (partial-start make-general-fade colour-fade))
    (else
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Unrecognised attribute type")
                         (make-exception-with-irritants type))))))


(define (run-cue-index! pb cue-index)
  (let* ((this-cue-state (calculate-tracking (get-playback-cue-list pb) cue-index))
         (the-cue (vector-ref (get-playback-cue-list pb) cue-index))
         (overlay-state (make-empty-state))
         (cue-clock (make-clock #:expiration-time (cue-total-time the-cue))))

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
    (set-cue-clock! pb cue-clock)
    (atomic-box-set! (state-box pb) 'running)
    (run-hook (state-change-hook pb) 'running)))


(define (print-playback pb)
  (format #t "Playback ~a:\n" pb)
  ;;(format #t "        Cue list ~a\n" (get-playback-cue-list pb))
  (if (< (get-next-cue-index pb)
         (vector-length (get-playback-cue-list pb)))
      (let ((the-cue (vector-ref (get-playback-cue-list pb)
                                 (get-next-cue-index pb))))
        (format #t "  Next cue index ~a (~a)\n"
                (get-next-cue-index pb)
                the-cue))
      (format #t "  End of cue list.\n"))
  *unspecified*)


;;; ******************** Cue lists ********************

(define-syntax cue-part
  (syntax-rules ()
    ((_ (fixtures ...) params ...)
     (make-cue-part-obj (list fixtures ...)
                        params ...))))


(define* (make-cue-part-obj attr-list
                            #:key
                            (up-time 5)
                            (down-time 5)
                            (attr-time 3)
                            (up-delay 0)
                            (down-delay 0)
                            (attr-delay 0))
  (make-cue-part attr-list
                 (make-fade-times
                  up-time
                  down-time
                  attr-time
                  up-delay
                  down-delay
                  attr-delay)))


(define cue
  (lambda (number state . rest)
    (receive (cue-parts rest-minus-cue-parts)
        (partition cue-part? rest)
      (let-keywords rest-minus-cue-parts #f
                    ((up-time 5)
                     (down-time 5)
                     (attr-time 5)
                     (up-delay 0)
                     (down-delay 0)
                     (attr-delay 0)
                     (preset-time 1)
                     (track-intensities #f))

                    (make-cue (qnum number)
                              state
                              #f
                              (make-fade-times
                               up-time
                               down-time
                               attr-time
                               up-delay
                               down-delay
                               attr-delay)
                              preset-time
                              track-intensities
                              cue-parts)))))


(define (ensure-cue-zero-realized the-cue-list)
  (let ((cue-zero (vector-ref the-cue-list 0)))
    (unless (get-tracked-state cue-zero)
      (parameterize ((current-state (make-empty-state)))
        (set-tracked-state! cue-zero (current-state))))))


;; Get the state for a cue, taking into account tracking etc
(define (calculate-tracking the-cue-list cue-index)

  (ensure-cue-zero-realized the-cue-list)

  (if (>= cue-index (vector-length the-cue-list))
      #f
      (let* ((the-cue (vector-ref the-cue-list cue-index))
             (rstate (get-tracked-state the-cue)))
        (or rstate
            (let ((previous-state (calculate-tracking the-cue-list (- cue-index 1))))
              (parameterize ((current-state (make-empty-state)))
                (apply-state previous-state)
                (unless (track-intensities the-cue)
                  (blackout!))
                (apply-state (get-cue-state the-cue))
                (set-tracked-state! the-cue (current-state))
                (current-state)))))))


(define-method (update-state! (pb <starlet-playback>))
  (when (and (get-cue-clock pb)
             (clock-expired? (get-cue-clock pb))
             (eq? 'running (atomic-box-ref (state-box pb))))
    (when (eq? 'running (atomic-box-compare-and-swap! (state-box pb)
                                                      'running
                                                      'ready))
      (run-hook (state-change-hook pb) 'ready))))


(define-syntax cue-list
  (syntax-rules ()
    ((_ body ...)
     (vector (cue 0
                  (make-empty-state)
                  #:up-time 0
                  #:down-time 0
                  #:attr-time 0
                  #:preset-time 0)
             body ...))))
