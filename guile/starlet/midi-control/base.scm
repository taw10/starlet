;;
;; starlet/midi-control/base.scm
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
(define-module (starlet midi-control base)
  #:use-module (oop goops)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:export (start-midi-control
            get-cc-value
            ccval->percent
            percent->ccval
            send-note-on
            send-note-off
            register-midi-note-callback!
            register-midi-cc-callback!
            remove-midi-callback!))


(define cc-arrays (make-atomic-box '()))
(define callback-list (make-atomic-box '()))
(define send-queue (make-atomic-box '()))


(define-class <midi-callback> (<object>)

  (type
   #:init-keyword #:type
   #:getter get-type)

  (channel
   #:init-keyword #:channel
   #:getter get-channel)

  (note-or-cc-number
   #:init-keyword #:note-or-cc-number
   #:getter get-note-or-cc-number)

  (callback
   #:init-keyword #:func
   #:getter get-callback-func))

(define (find-cc-callbacks channel cc-number)
  (filter (lambda (a)
            (and (eq? cc-number (get-note-or-cc-number a))
                 (eq? channel (get-channel a))
                 (eq? 'cc (get-type a))))
          (atomic-box-ref callback-list)))


(define (find-note-callbacks channel note-number)
  (filter (lambda (a)
            (and (eq? note-number (get-note-or-cc-number a))
                 (eq? channel (get-channel a))
                 (eq? 'note (get-type a))))
          (atomic-box-ref callback-list)))


(define (remove-midi-callback! callback)
  (atomic-box-set! callback-list
                   (delq callback
                         (atomic-box-ref callback-list))))


(define (register-midi-callback! type
                                 channel
                                 note-or-cc-number
                                 func)
  (let ((new-callback (make <midi-callback>
                        #:type type
                        #:channel (if channel channel default-channel)
                        #:note-or-cc-number note-or-cc-number
                        #:func func)))
    (atomic-box-set! callback-list
                     (cons new-callback
                           (atomic-box-ref callback-list)))
    new-callback))


(define* (register-midi-note-callback!
          #:key (channel #f) (note-number 1) (func #f) (unique #t))
  (when unique
    (for-each remove-midi-callback! (find-note-callbacks
                                      (if channel channel default-channel)
                                      note-number)))
  (register-midi-callback! 'note channel note-number func))


(define* (register-midi-cc-callback!
          #:key (channel #f) (cc-number 1) (func #f) (unique #t))
  (when unique
    (for-each remove-midi-callback! (find-cc-callbacks
                                      (if channel channel default-channel)
                                      cc-number)))
  (register-midi-callback! 'cc channel cc-number func))


(define enqueue-midi-bytes!
  (lambda bytes
    (let* ((old-queue (atomic-box-ref send-queue))
           (new-queue (append old-queue bytes)))
      (unless (eq? (atomic-box-compare-and-swap! send-queue
                                                 old-queue
                                                 new-queue)
                   old-queue)
        (apply enqueue-midi-bytes! bytes)))))


(define* (send-note-on note
                       #:key (channel #f))
  (when note
    (enqueue-midi-bytes! (+ #b10010000
                            (if channel channel default-channel))
                         note
                         127)))


(define* (send-note-off note
                        #:key (channel #f))
  (when note
    (enqueue-midi-bytes! (+ #b10000000
                            (if channel channel default-channel))
                         note
                         0)))


(define (all-notes-off! channel)
  (let again ((l 0))
    (enqueue-midi-bytes! (+ #b10000000 channel) l 0)
    (unless (= l 127)
      (again (+ l 1)))))


(define (ensure-cc-array channel)
  (let ((old-list (atomic-box-ref cc-arrays)))
    (unless (assq channel old-list)
      (unless (eq?
               old-list
               (atomic-box-compare-and-swap! cc-arrays
                                             old-list
                                             (acons channel
                                                    (make-vector 128 #f)
                                                    old-list)))
        ;; CAS failed - try again
        (ensure-cc-array channel)))))


(define (check-cc-callbacks channel cc-number old-val new-val)
  (for-each (lambda (a) ((get-callback-func a) old-val new-val))
            (find-cc-callbacks channel cc-number)))


(define (handle-cc-change! channel cc-number value)
  (ensure-cc-array channel)
  (let* ((cc-array (assq-ref (atomic-box-ref cc-arrays) channel))
         (old-value (vector-ref cc-array cc-number)))
    (vector-set! cc-array cc-number value)
    (check-cc-callbacks channel cc-number old-value value)))


(define* (get-cc-value cc-number
                       #:key (channel #f))
  (let ((cc-arrays (atomic-box-ref cc-arrays)))
    (let ((ccs (assq-ref cc-arrays
                         (if channel channel default-channel))))
      (if ccs
          (vector-ref ccs cc-number)
          #f))))


(define (check-note-callbacks channel note-number)
  (for-each (lambda (a) ((get-callback-func a)))
            (find-note-callbacks channel note-number)))


(define (ccval->percent n)
  (/ (* n 100) 127))


(define (percent->ccval n)
  (inexact->exact (round (/ (* n 127) 100))))


(define default-channel 0)

(define (start-midi-control-real device-name channel)
  (let ((midi-port (open-file device-name "r+0b")))

    ;; Read thread
    (begin-thread
      (with-exception-handler
        (lambda (exn)
          (backtrace)
          (raise-exception exn))
        (lambda ()
          (let again ()

            (let* ((status-byte (get-u8 midi-port))
                   (channel (bit-extract status-byte 0 4))
                   (command (bit-extract status-byte 4 8)))

              (case command

                ;; Note on
                ((9) (let* ((note (get-u8 midi-port))
                            (vel (get-u8 midi-port)))
                       (check-note-callbacks channel note)))

                ;; Control value
                ((11) (let* ((cc-number (get-u8 midi-port))
                             (value (get-u8 midi-port)))
                        (handle-cc-change! channel
                                           cc-number
                                           value))))

              (yield)
              (again))))))

    ;; Write thread
    (begin-thread
      (let again ()
        (let ((bytes-to-send (atomic-box-swap! send-queue '())))
          (for-each (lambda (a)
                      (put-u8 midi-port a)
                      (usleep 1))
                    bytes-to-send)
          (usleep 1000)
          (again))))

    (all-notes-off! default-channel)))


(define midi-running #f)

(define (start-dummy-midi)
  (display "Using dummy MIDI control\n")
  (begin-thread
    (let again ()
      (let ((bytes-to-send (atomic-box-swap! send-queue '())))
        (usleep 1000)
        (again))))
  (set! midi-running #t))

(define* (start-midi-control device-name
                             #:key (channel #f))


  (if midi-running

      (format #t "MIDI already running\n")

      (begin
        (when channel
          (set! default-channel channel))

        (with-exception-handler

          (lambda (exn)
            (format #t "Couldn't start MIDI ~a\n"
                    (exception-irritants exn))
            (start-dummy-midi))

          (lambda ()
            (start-midi-control-real device-name channel)
            (set! midi-running #t))

          #:unwind? #t))))
