;;
;; starlet/cue-list.scm
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
  #:export (cue
            cue-part
            cue-list
            cue-number-to-index
            cue-index-to-number
            current-cue-clock
            read-cue-list-file)
  #:re-export (snap))


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
            transition-effect
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
  (transition-effect  get-transition-effect)
  (track-intensities  track-intensities)
  (cue-parts          get-cue-parts)
  (cue-clock          get-cue-clock))


(define (qnum a)
  (/ (inexact->exact (* a 1000)) 1000))


(define (cue-index-to-number cue-list cue-index)
  (get-cue-number (vector-ref cue-list cue-index)))


(define (cue-number-to-index cue-list cue-number)
  (vector-index (lambda (a)
                  (eqv? (get-cue-number a)
                        cue-number))
                cue-list))


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


(define-syntax cue-part
  (syntax-rules ()
    ((_ (fixtures ...) params ...)
     (make-cue-part-obj (list fixtures ...)
                        params ...))))


;; FIXME!
(define (cue-total-time the-cue)
  100)

(define (cue-proc number . args)
  (receive
    (states transition-effects cue-parts rest)
    (categorize args lighting-state? transition-effect? cue-part?)
    (let-keywords
      rest
      #f  ;; allow-other-keys?
      ((track-intensities #f))

      (when (> (length states) 1)
        (error "A cue can only contain one state"))

      (when (> (length transition-effects) 1)
        (error "A cue can only contain one transition effect"))

      (let ((the-cue (make-cue (qnum number)
                               (car states)
                               #f   ;; tracked state, to be filled later
                               #f   ;; preset state, to be filled later
                               (car transition-effects)
                               track-intensities
                               cue-parts
                               (current-cue-clock))))

        (set-clock-expiration-time! (current-cue-clock)
                                    (cue-total-time the-cue))
        the-cue))))


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


(define (dark? a)
  (or (eq? a 'no-value)
      (and (number? a)
           (< a 1))))


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


(define-syntax cue-list
  (syntax-rules ()
    ((_ body ...)
     (let ((the-cue-list (vector (cue 0
                                      (make-empty-state)
                                      (snap))
                                 body ...)))
       (track-all-cues! the-cue-list)
       (preset-all-cues! the-cue-list)
       the-cue-list))))


(define (read-cue-list-file filename)
  (call-with-input-file
    filename
    (lambda (port)
      (eval (read port) (interaction-environment)))))
