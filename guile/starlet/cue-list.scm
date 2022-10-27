;;
;; starlet/cue-list.scm
;;
;; Copyright © 2020-2022 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet cue-list)
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
  #:use-module (starlet clock)
  #:use-module (starlet utils)
  #:use-module (starlet transition-effect)
  #:use-module (starlet snap-transition)
  #:use-module (starlet crossfade)
  #:export (cue
            cue-part
            cue-list
            qnum
            get-cue-number
            get-cue-parts
            get-cue-clock
            get-preset-state
            get-cue-part-state
            get-cue-part-transition
            cue-number-to-index
            cue-index-to-number
            current-cue-clock
            read-cue-list-file
            num-cues)
  #:re-export (snap crossfade))


(define-record-type <cue-part>
  (cue-part state transition)
  cue-part?
  (state        get-cue-part-state
                set-cue-part-state!)
  (transition   get-cue-part-transition))


(define-record-type <cue>
  (make-cue number
            preset-state
            track-intensities
            cue-parts
            cue-clock)
  cue?
  (number             get-cue-number)
  (preset-state       get-preset-state
                      set-preset-state!)
  (track-intensities  track-intensities)
  (cue-parts          get-cue-parts)
  (cue-clock          get-cue-clock))


(define-method (num-cues (l <vector>))
  (vector-length l))


(define (qnum a)
  (/ (inexact->exact (* a 1000)) 1000))


(define (cue-index-to-number cue-list cue-index)
  (get-cue-number (vector-ref cue-list cue-index)))


(define (cue-number-to-index cue-list cue-number)
  (vector-index (lambda (a)
                  (eqv? (get-cue-number a)
                        cue-number))
                cue-list))


(define (fix-attr-eq fa1 fa2)
  (and (eq? (car fa1) (car fa2))
       (eq? (cdr fa1) (cdr fa2))))


(define (fix-attrs-in-state state)
  (state-map->list
    (lambda (fix attr val) (cons fix attr))
    state))


(define (add-fix-attrs-to-list state old-list)
  (lset-union fix-attr-eq
              old-list
              (fix-attrs-in-state state)))


(define (cue-proc number . args)
  (receive
    (states transition-effects cue-parts rest)
    (categorize args lighting-state? transition-effect? cue-part?)

    (let-keywords
      rest
      #f  ;; allow-other-keys?
      ((track-intensities #f))

      (let ((n-tr-effs (length transition-effects))
            (n-states (length states)))

        (make-cue (qnum number)
                  #f   ;; preset state, to be filled later
                  track-intensities

                  ;; Create the list of cue parts
                  (cond

                    ;; Only explicitly-stated cue parts
                    [(= 0 n-tr-effs n-states)
                     cue-parts]

                    ;; Implicit first cue part
                    [(= 1 n-tr-effs n-states)
                     (cons
                       (cue-part (car states)
                                 (car transition-effects))
                       cue-parts)]

                    ;; Wrong number of states or transition effects
                    [(not (= n-states 1))
                     (error "Cue must contain exactly one state: " number)]
                    [(not (= n-tr-effs 1))
                     (error "Cue must contain exactly one transition effect: " number)])

                  (current-cue-clock))))))


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
                                 (apply-state
                                   (get-cue-part-state
                                     (car (get-cue-parts the-cue)))))))
        (set-cue-part-state! (car (get-cue-parts the-cue))
                             the-tracked-state)
        (lighting-state
          (apply-state the-tracked-state)
          (for-each
            (lambda (part)
              (apply-state (get-cue-part-state part)))
            (cdr (get-cue-parts the-cue))))))
    (make-empty-state)
    the-cue-list))


(define (dark? a)
  (or (eq? a 'no-value)
      (and (number? a)
           (< a 1))))


(define (fixture-dark-in-cue? fix the-cue)
  (every
    (lambda (part)
      (dark? (state-find fix 'intensity (get-cue-part-state part))))
    (get-cue-parts the-cue)))


(define-syntax for-each-cue-part
  (syntax-rules ()
    ((_ the-cue (part) body ...)
     (for-each
       (lambda (part)
         body ...)
       (get-cue-parts the-cue)))))


(define-syntax for-every-attr-in-cue
  (syntax-rules ()
    ((_ the-cue (fix attr val) body ...)
     (for-each-cue-part
       the-cue (part)
       (state-for-each
         (lambda (fix attr val)
           body ...)
         (get-cue-part-state part))))))


(define (preset-all-cues! the-cue-list)
  (let loop ((idx 0))
    (let ((the-cue (vector-ref the-cue-list idx))
          (next-cue (vector-ref the-cue-list (1+ idx)))
          (preset-state (make-empty-state)))
      (for-every-attr-in-cue
        next-cue (fix attr val)
        (unless (intensity? attr)
          (when (fixture-dark-in-cue? fix the-cue)
            (set-in-state! preset-state fix attr val))))
      (set-preset-state! the-cue preset-state))
    (if (< (+ 2 idx) (vector-length the-cue-list))
      (loop (1+ idx))
      (set-preset-state!
        (vector-ref the-cue-list (1+ idx))
        (make-empty-state)))))


(define-syntax cue-list
  (syntax-rules ()
    ((_ body ...)
     (let ((the-cue-list
             (list->vector
               (remove unspecified?
                       (list
                         (cue 0
                              (make-empty-state)
                              (snap))
                         body ...)))))
       (track-all-cues! the-cue-list)
       (preset-all-cues! the-cue-list)
       the-cue-list))))


(define (read-cue-list-file filename)
  (call-with-input-file
    filename
    (lambda (port)
      (eval (read port) (interaction-environment)))))
