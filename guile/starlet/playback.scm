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
  #:use-module (srfi srfi-26)
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
            cut!
            stop!
            back!
            cue-list
            reload-cue-list!
            reassert-current-cue!
            print-playback
            state-change-hook
            current-cue-clock))


;; A "playback" is a state which knows how to run cues
;; from a cue list
(define-class <starlet-playback> (<starlet-state>)
  (cue-list
    #:init-keyword #:cue-list
    #:getter get-playback-cue-list
    #:setter set-playback-cue-list!)

  (cue-list-file
    #:init-keyword #:cue-list-file
    #:getter get-playback-cue-list-file
    #:setter set-playback-cue-list-file!)

  (next-cue-index
    #:init-value 0
    #:getter get-next-cue-index
    #:setter set-next-cue-index!)

  (running-cue-clock
    #:init-value #f
    #:getter get-pb-cue-clock
    #:setter set-pb-cue-clock!)

  (running-cue
    #:init-value #f
    #:getter get-running-cue
    #:setter set-running-cue!)

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
            preset-state
            fade-times
            preset-time
            track-intensities
            cue-parts
            cue-clock)
  cue?
  (number             get-cue-number)
  (state              get-cue-state)
  (tracked-state      get-tracked-state
                      set-tracked-state!)
  (preset-state       get-preset-state
                      set-preset-state!)
  (fade-times         get-cue-fade-times)
  (preset-time        get-cue-preset-time)
  (track-intensities  track-intensities)
  (cue-parts          get-cue-parts)
  (cue-clock          get-cue-clock))


(define (get-playback-cue-number pb)
  (cue-index-to-number (get-playback-cue-list pb)
                       (max 0 (- (get-next-cue-index pb) 1))))

(define (qnum a)
  (/ (inexact->exact (* a 1000)) 1000))


(define (read-cue-list-file filename)
  (call-with-input-file
    filename
    (lambda (port)
      (eval (read port) (interaction-environment)))))


(define (reload-cue-list! pb)
  (let ((filename (get-playback-cue-list-file pb)))
    (if filename

      (let ((new-cue-list (read-cue-list-file filename))
            (current-cue-number (get-playback-cue-number pb)))
        (set-playback-cue-list! pb new-cue-list)
        (let ((new-current-cue-index (cue-number-to-index
                                       new-cue-list
                                       current-cue-number)))
          (if new-current-cue-index
            (set-next-cue-index! pb (+ new-current-cue-index 1))
            (begin
              (display "Current cue no longer exists!\n")
              (display "Use run-cue-number! or cut-to-cue-number! to resume.\n")
              (set-next-cue-index! pb #f))))

        'cue-list-reloaded)

      'playback-without-cue-list-file)))


(define* (make-playback #:key
                        (cue-list-file #f)
                        (cue-list #f))
  (let ((new-playback (make <starlet-playback>
                            #:cue-list (if cue-list-file
                                         (read-cue-list-file cue-list-file)
                                         cue-list)
                            #:cue-list-file cue-list-file)))
    (register-state! new-playback)
    new-playback))


(define (cue-index-to-number cue-list cue-index)
  (get-cue-number (vector-ref cue-list cue-index)))


(define (cue-number-to-index cue-list cue-number)
  (vector-index (lambda (a)
                  (eqv? (get-cue-number a)
                        cue-number))
                cue-list))


(define (set-playback-state! pb state)
  (atomic-box-set! (state-box  pb) state)
  (run-hook (state-change-hook pb) state))


(define (cut-to-cue-index! pb cue-index)
  (clear-state! pb)
  (set-next-cue-index! pb (+ cue-index 1))
  (set-pb-cue-clock! pb #f)
  (set-running-cue! pb #f)
  (set-playback-state! pb 'ready)

  ;; Set the actual state
  (state-for-each
    (lambda (fix attr val)
      (set-in-state! pb fix attr (lambda () val)))
    (get-tracked-state (vector-ref (get-playback-cue-list pb)
                                   cue-index)))

  ;; Set the preset state on top
  (state-for-each
    (lambda (fix attr val)
      (set-in-state! pb fix attr (lambda () val)))
    (get-preset-state (vector-ref (get-playback-cue-list pb)
                                  cue-index))))


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
  (let ((clock (get-pb-cue-clock pb)))
    (if (and clock
             (clock-stopped? clock))

      ;; Restart paused cue
      (begin (start-clock! clock)
             (set-playback-state! pb 'running))

      ;; Run next cue
      (if (get-next-cue-index pb)

        (let ((next-cue-index (get-next-cue-index pb)))
          (if (< next-cue-index (vector-length (get-playback-cue-list pb)))
            (begin
              (run-cue-index! pb next-cue-index)
              (set-next-cue-index! pb (+ next-cue-index 1))
              *unspecified*)
            'no-more-cues-in-list))

        'next-cue-unspecified))))


(define (cut! pb)
  (cut-to-cue-index! pb (get-next-cue-index pb)))


(define (stop! pb)
  (let ((clock (get-pb-cue-clock pb)))
    (when (and clock
               (not (clock-expired? clock)))
      (stop-clock! (get-pb-cue-clock pb))
      (set-playback-state! pb 'pause))))


(define (back! pb)
  (if (get-next-cue-index pb)
    (let ((prev-cue-index (- (get-next-cue-index pb) 2)))
      (if (>= prev-cue-index 0)
        (begin (cut-to-cue-index! pb prev-cue-index)
               (set-playback-state! pb 'ready))
        'already-at-cue-zero))
    'next-cue-unspecified))


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
    (start-clock! cue-clock)
    (set-playback-state! pb 'running)))


(define (print-playback pb)
  (format #t "Playback ~a:\n" pb)
  ;;(format #t "        Cue list ~a\n" (get-playback-cue-list pb))
  (if (get-next-cue-index pb)
    (if (< (get-next-cue-index pb)
           (vector-length (get-playback-cue-list pb)))
      (let ((the-cue (vector-ref (get-playback-cue-list pb)
                                 (get-next-cue-index pb))))
        (format #t "  Next cue index ~a (~a)\n"
                (get-next-cue-index pb)
                the-cue))
      (format #t "  End of cue list.\n"))
    (format #t "  Next cue index is unspecified.\n"))
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
                            (attr-time 0)
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


(define cue-proc
  (lambda (number state . rest)
    (receive (cue-parts rest-minus-cue-parts)
             (partition cue-part? rest)
             (let-keywords rest-minus-cue-parts #f
                           ((up-time 5)
                            (down-time 5)
                            (attr-time 0)
                            (up-delay 0)
                            (down-delay 0)
                            (attr-delay 0)
                            (preset-time 1)
                            (track-intensities #f))

                           (let ((the-cue (make-cue (qnum number)
                                                    state
                                                    #f   ;; tracked state
                                                    #f   ;; preset state
                                                    (make-fade-times
                                                      up-time
                                                      down-time
                                                      attr-time
                                                      up-delay
                                                      down-delay
                                                      attr-delay)
                                                    preset-time
                                                    track-intensities
                                                    cue-parts
                                                    (current-cue-clock))))

                             (set-clock-expiration-time! (current-cue-clock)
                                                         (cue-total-time the-cue))
                             the-cue)))))


(define current-cue-clock (make-parameter #f))

(define-syntax cue
  (syntax-rules ()
    ((_ body ...)
     (parameterize ((current-cue-clock (make-clock #:stopped #t)))
       (cue-proc body ...)))))


(define (track-all-cues! the-cue-list)
  (vector-fold
    (lambda (idx prev-state the-cue)
      (let ((the-tracked-state (lighting-state
                                 (apply-state prev-state)
                                 (unless (track-intensities the-cue)
                                   (blackout!))
                                 (apply-state (get-cue-state the-cue)))))
        (set-tracked-state! the-cue the-tracked-state)
        the-tracked-state))
    (make-empty-state)
    the-cue-list))


(define (fixture-dark-in-state? fix state)
  (dark? (state-find fix 'intensity state)))


(define (preset-all-cues! the-cue-list)
  (vector-fold-right
    (lambda (idx next-state the-cue)
      (let ((preset-state (make-empty-state)))

        (state-for-each
          (lambda (fix attr val)
            (unless (intensity? attr)
              (when (fixture-dark-in-state? fix (get-tracked-state the-cue))
                (set-in-state! preset-state fix attr val))))
          next-state)

        (set-preset-state! the-cue preset-state))

      ;; Pass the raw state from this cue to the previous one
      (get-cue-state the-cue))

    (make-empty-state)
    the-cue-list))


(define-method (update-state! (pb <starlet-playback>))
  (when (and (get-pb-cue-clock pb)
             (clock-expired? (get-pb-cue-clock pb))
             (eq? 'running (atomic-box-ref (state-box pb))))
    (when (eq? 'running (atomic-box-compare-and-swap! (state-box pb)
                                                      'running
                                                      'ready))
      (run-hook (state-change-hook pb) 'ready)
      (let ((st (get-preset-state (get-running-cue pb))))
        (state-for-each
          (lambda (fix attr val)
            (set-in-state! pb fix attr (lambda () val)))
          st))
      (set-running-cue! pb #f))))


(define-syntax cue-list
  (syntax-rules ()
    ((_ body ...)
     (let ((the-cue-list (vector (cue 0
                                      (make-empty-state)
                                      #:up-time 0
                                      #:down-time 0
                                      #:attr-time 0
                                      #:preset-time 0)
                                 body ...)))
       (track-all-cues! the-cue-list)
       (preset-all-cues! the-cue-list)
       the-cue-list))))


(define (reassert-current-cue! pb)
  (cut-to-cue-number! pb (get-playback-cue-number pb)))
