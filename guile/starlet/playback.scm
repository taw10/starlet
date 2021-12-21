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
  #:use-module (starlet cue-list)
  #:use-module (starlet colours)
  #:use-module (starlet transition-effect)
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


(define-class <transition-effect> (<object>)
  (func
    #:init-value #f))


(define (transition-effect? a)
  (is-a? a <transition-effect>))


(define-record-type <cue-part>
  (make-cue-part attr-list transition)
  cue-part?
  (attr-list    get-cue-part-attr-list)
  (transition   get-cue-part-transition))


(define-record-type <cue>
  (make-cue number
            state
            tracked-state
            preset-state
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
  (track-intensities  track-intensities)
  (cue-parts          get-cue-parts)
  (cue-clock          get-cue-clock))


(define (get-playback-cue-number pb)
  (let ((cue-idx (get-next-cue-index pb)))
    (if cue-idx
      (cue-index-to-number (get-playback-cue-list pb)
                           (max 0 (- cue-idx 1)))
      #f)))

(define (qnum a)
  (/ (inexact->exact (* a 1000)) 1000))


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


(define (run-cue-index! pb cue-index)
  (let* ((the-cue (vector-ref (get-playback-cue-list pb) cue-index))
         (this-cue-state (get-tracked-state the-cue))
         (overlay-state (make-empty-state))
         (cue-clock (get-cue-clock the-cue)))

    ;; FIXME: Use transition effect

    (atomically-overlay-state! pb overlay-state)
    (set-pb-cue-clock! pb cue-clock)
    (set-running-cue! pb the-cue)
    (reset-clock! cue-clock)
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


(define (reassert-current-cue! pb)
  (cut-to-cue-number! pb (get-playback-cue-number pb)))
