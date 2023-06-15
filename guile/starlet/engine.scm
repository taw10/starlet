;;
;; starlet/engine.scm
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
(define-module (starlet engine)
  #:use-module (starlet fixture)
  #:use-module (starlet state)
  #:use-module (starlet utils)
  #:use-module (starlet attributes)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:export (patch-fixture!
            patch-many!
            engine-freq
            total-num-attrs
            register-state!
            current-value
            current-value-state
            patched-fixture-names
            patched-fixtures))


;; The list of patched fixtures
(define fixtures (make-atomic-box '()))

;; List of states being scanned out
(define state-list (make-atomic-box '()))

;; Association list of names to states
(define state-names (make-atomic-box '()))

;; Current values (literal, not functions) of active attributes
(define current-values (make-atomic-box (make-empty-state)))


(define (patched-fixture-names)
  (map get-fixture-name (atomic-box-ref fixtures)))


(define (current-value-state)
  (atomic-box-ref current-values))


(define (patched-fixtures)
  (atomic-box-ref fixtures))


(define (total-num-attrs)
  (fold (lambda (fix prev)
          (+ prev (length (get-fixture-attrs fix))))
        0
        (atomic-box-ref fixtures)))


(define (get-state-name st)
  (assq-ref (atomic-box-ref state-names)
            st))


(define (set-state-name! st name)
  (atomic-box-set! state-names
                   (assq-set! (atomic-box-ref state-names)
                              st
                              name)))


;; Patch a new fixture
(define* (patch-real name
                     class
                     start-addr
                     #:key (universe 0) (friendly-name "Fixture"))
  (let ((new-fixture (make class
                           #:name name
                           #:sa start-addr
                           #:uni universe
                           #:friendly-name friendly-name)))
    (atomic-box-set! fixtures (cons new-fixture
                                    (atomic-box-ref fixtures)))
    new-fixture))


(define-syntax patch-fixture!
  (syntax-rules ()
    ((_ name stuff ...)
     (define name (patch-real (quote name) stuff ...)))))


;; Patch several new fixtures
(define* (patch-many-real name
                          class
                          start-addrs
                          #:key (universe 0) (friendly-name "Fixture"))
  (map (lambda (start-addr n)
         (patch-real `(list-ref ,name ,n)
                     class
                     start-addr
                     #:universe universe
                     #:friendly-name friendly-name))
       start-addrs
       (iota (length start-addrs))))


(define-syntax patch-many!
  (syntax-rules ()
    ((_ name stuff ...)
     (define name (patch-many-real (quote name) stuff ...)))))


(define-method (current-value (fix <fixture>) (attr-name <starlet-attribute>))
  (let ((v (state-find fix attr-name (current-value-state))))
    (if (eq? v 'no-value)
      (get-attr-home-val fix attr-name)
      v)))


(define (append-or-replace-named-state orig-list name new-state)
  (let ((new-list (map (lambda (st)
                         (if (eq? (get-state-name st) name)
                             (begin
                               new-state)
                             st))
                       orig-list)))

    ;; If there is no state with this name in the list,
    ;; the replacement above will have no effect.
    ;; Check again and add in the normal way if so.
    (if (find (lambda (st) (eq? (get-state-name st)
                                name))
              new-list)
        new-list
        (append orig-list (list new-state)))))


(define* (register-state! new-state
                          #:key (unique-name #f))
  (if unique-name
      (begin (set-state-name! new-state unique-name)
             (atomic-box-set! state-list
                              (append-or-replace-named-state (atomic-box-ref state-list)
                                                             unique-name
                                                             new-state)))
      (atomic-box-set! state-list
                       (append (atomic-box-ref state-list)
                               (list new-state)))))


(define engine-thread #f)
(define engine-freq 0)


(define (htp-attr? attr)
  (eq? attr intensity))


(define (engine-loop start-time count)

  ;; Combine all the active attributes and send it out
  (atomic-box-swap! current-values
    (combine-states
      (let ((states (atomic-box-ref state-list)))
        (for-each update-state! states)
        (fold
          (lambda (incoming-state combined-state)
            (state-for-each
              (lambda (fix attr val)
                (let ((incoming-val (value->number val))
                      (current-val (state-find fix attr combined-state)))
                  (unless (eq? incoming-val 'no-value)
                    (if (eq? current-val 'no-value)
                      (set-in-state! combined-state fix attr incoming-val)
                      (set-in-state! combined-state fix attr
                                     (if (htp-attr? attr)
                                       (max incoming-val current-val)
                                       incoming-val))))))
              incoming-state)
            combined-state)
          (make-empty-state)
          states))
      programmer-state))

  (usleep 20000)

  ;; Update output rate every 1000 cycles
  (if (eq? count 100)
    (begin
      (set! engine-freq
        (exact->inexact (/ 100
                           (- (hirestime) start-time))))
      (engine-loop (hirestime) 0))
    (engine-loop start-time (+ count 1))))


(define (start-engine)
  (if engine-thread
    (format #t "Engine thread is already running\n")
    (let ((start-time (hirestime)))
      (set! engine-thread
        (begin-thread
          (with-exception-handler
            (lambda (exn)
              (display "Error in engine thread:\n")
              (set! engine-thread #f)
              (backtrace)
              (raise-exception exn))
            (lambda ()
              (engine-loop start-time 0))
            #:unwind? #f))))))


(start-engine)
