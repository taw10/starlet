;;
;; starlet/scanout.scm
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
(define-module (starlet scanout)
  #:use-module (starlet fixture)
  #:use-module (starlet state)
  #:use-module (starlet utils)
  #:use-module (starlet colours)
  #:use-module (starlet guile-ola)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:export (patch-fixture!
            patch-many!
            scanout-freq
            total-num-attrs
            register-state!
            current-value
            patched-fixture-names
            get-attr
            set-chan8
            set-chan16
            scanout-fixture))


;; The list of patched fixtures
(define fixtures (make-atomic-box '()))

;; List of states being scanned out
(define state-list (make-atomic-box '()))

;; Association list of names to states
(define state-names (make-atomic-box '()))

;; Method for scanning out fixtures
(define-generic scanout-fixture)


(define (patched-fixture-names)
  (map get-fixture-name (atomic-box-ref fixtures)))


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


(define (state-has-fix-attr fix attr state)
  (let ((val (state-find fix attr state)))
    (if (eq? 'no-value val)
        #f
        (not (eq? 'no-value (value->number val))))))


(define (first-val fix attr state-list)
  (let ((first-state (find (lambda (state)
                             (state-has-fix-attr fix attr state))
                           state-list)))
    (if first-state
        (state-find fix attr first-state)
        'no-value)))


(define-method (current-value (fix <fixture>) (attr-name <symbol>))
  (let  ((programmer-val (state-find fix attr-name programmer-state)))
    (if (eq? 'no-value programmer-val)

        ;; Look in the states
        (if (intensity? attr-name)

            ;; HTP for intensity
            (fold (lambda (state prev)
                    (let ((val (state-find fix attr-name state)))
                      (if (eq? 'no-value val)
                          prev
                          (let ((real-val (value->number val)))
                            (if (eq? 'no-value real-val)
                                prev
                                (max real-val prev))))))
                  0.0
                  (atomic-box-ref state-list))

            ;; Priority order for everything else
            (let ((val (first-val fix attr-name (atomic-box-ref state-list))))
              (if (eq? 'no-value val)
                  (get-attr-home-val fix attr-name)
                  (value->number val))))

        ;; Use programmer value, if we have it
        (value->number programmer-val))))


(define-method (current-value (fix <fixture>) (attr-name <colour-component-id>))
  (let ((colour (current-value fix 'colour)))
    (extract-colour-component colour attr-name)))


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


(define (send-to-ola ola-client universe-buffer-pair)
  (let ((uni (car universe-buffer-pair))
        (buf (cdr universe-buffer-pair)))
  (send-streaming-dmx-data! ola-client uni buf)))


(define scanout-freq 0)
(define ola-thread #f)
(define current-scanout-fixture (make-parameter #f))
(define current-scanout-universe (make-parameter #f))
(define current-scanout-addr (make-parameter #f))


(define (get-attr attr-name)
  (current-value
    (current-scanout-fixture)
    attr-name))


(define (set-dmx universe addr value)
  (ensure-number value (list universe addr value))

  ;; Create DMX array for universe if it doesn't exist already
  (set-ola-dmx-buffer! universe
                       (- addr 1)              ; OLA indexing starts from zero
                       (round-dmx value)))


(define (set-chan8 relative-channel-number value)
  (ensure-number
    value
    (list (current-scanout-fixture)
          relative-channel-number
          value))
  (set-dmx
    (current-scanout-universe)
    (+ (current-scanout-addr)
       relative-channel-number
       -1)
    value))


(define (set-chan16 relative-channel-number value)
  (ensure-number
    value
    (list (current-scanout-fixture)
          relative-channel-number
          value))
  (set-chan8 relative-channel-number (msb value))
  (set-chan8 (+ relative-channel-number 1) (lsb value)))


(define (scanout-loop ola-client start-time count previous-universes)

  (let ((universes '()))

    (for-each update-state! (atomic-box-ref state-list))

    (for-each
      (lambda (fix)

        ;; Ensure the DMX array exists for this fixture's universe
        (unless (assq (get-fixture-universe fix) universes)
          (set! universes (acons (get-fixture-universe fix)
                                 (make-ola-dmx-buffer)
                                 universes)))

        (parameterize
          ((current-scanout-fixture fix)
           (current-scanout-universe (assq-ref
                                       universes
                                       (get-fixture-universe fix)))
           (current-scanout-addr (get-fixture-addr fix)))
          (scanout-fixture fix)))
      (atomic-box-ref fixtures))

    ;; Send everything to OLA
    (for-each (lambda (uni-buf-pair)
                (let ((uni (car uni-buf-pair))
                      (buf (cdr uni-buf-pair)))
                  (let ((prev-buf (assv-ref previous-universes uni)))

                    ;; Do not send exactly the same data every time,
                    ;; but do send an update once every 100 loops, just to
                    ;; make sure OLA does not forget about us.
                    (unless (and prev-buf
                                 (ola-dmx-buffers-equal? buf prev-buf)
                                 (not (= count 0)))
                      (send-streaming-dmx-data! ola-client uni buf)))))
              universes)

    (usleep 10000)

    ;; Update scanout rate every 1000 cycles
    (if (eq? count 100)
      (begin
        (set! scanout-freq
          (exact->inexact (/ 100
                             (- (hirestime) start-time))))
        (scanout-loop ola-client (hirestime) 0 universes))
      (scanout-loop ola-client start-time (+ count 1) universes))))


(define (start-ola-output)
  (if ola-thread
      (format #t "OLA output already running\n")
      (let* ((ola-client (make-ola-streaming-client))
             (start-time (hirestime)))

        (set! ola-thread
          (begin-thread
            (with-exception-handler
              (lambda (exn)
                (display "Error in OLA output thread:\n")
                (set! ola-thread #f)
                (backtrace)
                (raise-exception exn))
              (lambda ()
                (scanout-loop ola-client start-time 0 '()))
              #:unwind? #f))))))


(start-ola-output)
