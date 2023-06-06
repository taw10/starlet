;;
;; starlet/scanout.scm
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
(define-module (starlet scanout)
  #:use-module (starlet engine)
  #:use-module (starlet fixture)
  #:use-module (starlet state)
  #:use-module (starlet utils)
  #:use-module (starlet colours)
  #:use-module (starlet attributes)
  #:use-module (starlet guile-ola)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:export (scanout-freq
            get-attr
            set-chan8
            set-chan16))


(define scanout-thread #f)
(define scanout-freq 0)

(define current-scanout-fixture (make-parameter #f))
(define current-scanout-universe (make-parameter #f))
(define current-scanout-addr (make-parameter #f))
(define current-scanout-state (make-parameter (make-empty-state)))


(define-method (get-attr (attr-name <starlet-attribute>))
  (let ((v (state-find (current-scanout-fixture)
                       attr-name
                       (current-scanout-state))))
    (if (eq? v 'no-value)
      (get-attr-home-val (current-scanout-fixture) attr-name)
      v)))


(define-method (get-attr (attr-name <colour-component-id>))
  (extract-colour-component (get-attr colour) attr-name))


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


(define (scanout-loop ola-client start-time previous-universes count)

  (let ((universes '()))

    (parameterize
      ((current-scanout-state (current-value-state)))
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

        (patched-fixtures)))

    (for-each
      (lambda (uni-buf-pair)
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

    (usleep 20000)

    ;; Update output rate every 1000 cycles
    (if (eq? count 100)
      (begin
        (set! scanout-freq
          (exact->inexact (/ 100
                             (- (hirestime) start-time))))
        (scanout-loop ola-client (hirestime) universes 0))
      (scanout-loop ola-client start-time universes (+ count 1)))))


(define (start-scanout)
  (if scanout-thread
    (format #t "Scanout thread is already running\n")
    (let ((start-time (hirestime))
          (ola-client (make-ola-streaming-client)))
      (set! scanout-thread
        (begin-thread
          (with-exception-handler
            (lambda (exn)
              (display "Error in scanout thread:\n")
              (set! scanout-thread #f)
              (backtrace)
              (raise-exception exn))
            (lambda ()
              (scanout-loop ola-client start-time '() 0))
            #:unwind? #f))))))


(start-scanout)
