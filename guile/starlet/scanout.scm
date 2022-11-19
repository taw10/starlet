;;
;; starlet/scanout.scm
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
(define-module (starlet scanout)
  #:use-module (starlet fixture)
  #:use-module (starlet state)
  #:use-module (starlet utils)
  #:use-module (starlet engine)
  #:use-module (starlet guile-ola)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:export (start-ola-output
             scanout-freq
             get-attr
             set-chan8
             set-chan16))


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
