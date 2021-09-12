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
  #:export (make-midi-controller
            get-cc-value
            ccval->percent
            percent->ccval
            send-note-on
            send-note-off
            register-midi-note-callback!
            register-midi-cc-callback!
            remove-midi-callback!
            get-parameter-controller
            get-controller-sensitivity
            set-parameter-controller!
            make-sensitivity-knob))


(define-class <midi-control-surface> (<object>)
  (cc-values
    #:init-form (make-vector 128 #f)
    #:getter get-cc-values)

  (channel
    #:init-form (error "MIDI channel must be specified for controller")
    #:init-keyword #:channel
    #:getter get-channel)

  (callbacks
    #:init-form (make-atomic-box '())
    #:getter get-callbacks)

  (send-queue
    #:init-form (make-atomic-box '())
    #:getter get-send-queue)

  (parameter-controller
    #:init-value #f
    #:getter get-parameter-controller
    #:setter set-parameter-controller!)

  (sensitivity
    #:init-value 3
    #:getter get-controller-sensitivity
    #:setter set-controller-sensitivity!))


(define-class <midi-callback> (<object>)
  (type
   #:init-keyword #:type
   #:getter get-type)

  (note-or-cc-number
   #:init-keyword #:note-or-cc-number
   #:getter get-note-or-cc-number)

  (callback
   #:init-keyword #:func
   #:getter get-callback-func))


(define (find-cc-callbacks controller cc-number)
  (filter (lambda (a)
            (and (eq? cc-number (get-note-or-cc-number a))
                 (eq? 'cc (get-type a))))
          (atomic-box-ref (get-callbacks controller))))


(define (find-note-callbacks controller note-number)
  (filter (lambda (a)
            (and (eq? note-number (get-note-or-cc-number a))
                 (eq? 'note (get-type a))))
          (atomic-box-ref (get-callbacks controller))))


(define (remove-midi-callback! controller callback)
  (when controller
    (atomic-box-set! (get-callbacks controller)
                     (delq callback
                           (atomic-box-ref (get-callbacks controller))))))


(define (register-midi-callback! controller
                                 type
                                 note-or-cc-number
                                 func)
  (let ((new-callback (make <midi-callback>
                            #:type type
                            #:note-or-cc-number note-or-cc-number
                            #:func func)))
    (let ((callback-list-box (get-callbacks controller)))
      (atomic-box-set! callback-list-box
                       (cons new-callback
                             (atomic-box-ref callback-list-box))))
    new-callback))


(define* (register-midi-note-callback!
           controller
           #:key (note-number 1) (func #f) (unique #t))
  (when controller
    (when unique
      (for-each (lambda (callback)
                  (remove-midi-callback! controller callback))
                (find-note-callbacks
                  controller
                  note-number)))
    (register-midi-callback! controller 'note note-number func)))


(define* (register-midi-cc-callback!
           controller
           #:key (cc-number 1) (func #f) (unique #t))
  (when controller
    (when unique
      (for-each (lambda (callback)
                  (remove-midi-callback! controller callback))
                (find-cc-callbacks
                  controller
                  cc-number)))
    (register-midi-callback! controller 'cc cc-number func)))


(define enqueue-midi-bytes!
  (lambda (controller . bytes)
    (let* ((send-queue (get-send-queue controller))
           (old-queue (atomic-box-ref send-queue))
           (new-queue (append old-queue bytes)))
      (unless (eq? (atomic-box-compare-and-swap! send-queue
                                                 old-queue
                                                 new-queue)
                   old-queue)
        (apply enqueue-midi-bytes! (cons controller bytes))))))


(define* (send-note-on controller note)
  (when (and controller note)
    (enqueue-midi-bytes! controller
                         (+ #b10010000 (get-channel controller))
                         note
                         127)))


(define* (send-note-off controller note)
  (when (and controller note)
    (enqueue-midi-bytes! controller
                         (+ #b10000000 (get-channel controller))
                         note
                         0)))


(define (all-notes-off! controller)
  (for-each (lambda (l)
              (enqueue-midi-bytes! controller
                                   (+ #b10000000 (get-channel controller))
                                   l
                                   0))
            (iota 128)))


(define (check-cc-callbacks controller cc-number old-val new-val)
  (for-each (lambda (a) ((get-callback-func a) old-val new-val))
            (find-cc-callbacks controller cc-number)))


(define (handle-cc-change! controller cc-number value)
  (let* ((ccvals (get-cc-values controller))
         (old-value (vector-ref ccvals cc-number)))
    (vector-set! ccvals cc-number value)
    (check-cc-callbacks controller cc-number old-value value)))


(define* (get-cc-value controller cc-number)
  (if controller
    (vector-ref (get-cc-values controller) cc-number)
    #f))


(define (check-note-callbacks controller note-number)
  (for-each (lambda (a) ((get-callback-func a)))
            (find-note-callbacks controller note-number)))


(define (ccval->percent n)
  (/ (* n 100) 127))


(define (percent->ccval n)
  (inexact->exact (round (/ (* n 127) 100))))


(define (make-midi-controller-real device-name channel)
  (let ((controller (make <midi-control-surface>
                          #:channel channel)))
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
                         (check-note-callbacks controller note)))

                  ;; Control value
                  ((11) (let* ((cc-number (get-u8 midi-port))
                               (value (get-u8 midi-port)))
                          (handle-cc-change! controller
                                             cc-number
                                             value))))

                (yield)
                (again))))))

      ;; Write thread
      (begin-thread
        (let again ()
          (let ((bytes-to-send
                  (atomic-box-swap!
                    (get-send-queue controller)
                    '())))
            (for-each (lambda (a)
                        (put-u8 midi-port a)
                        (usleep 1))
                      bytes-to-send)
            (usleep 1000)
            (again))))

      (all-notes-off! controller)
      controller)))


(define* (make-midi-controller device-name channel)
  (with-exception-handler

    (lambda (exn)
      (format #t "Couldn't start MIDI ~a\n"
              (exception-irritants exn))
      #f)

    (lambda ()
      (make-midi-controller-real device-name channel))

    #:unwind? #t))


(define (set-sensitivity controller prev new)
  (set-controller-sensitivity!
    controller
    (min 5 (max 1 (+ (if (= new 127) -1 1)
                     (get-controller-sensitivity controller))))))


(define (make-sensitivity-knob controller cc-num)
  (register-midi-callback!
    controller 'cc cc-num
    (lambda (prev new)
      (set-sensitivity controller prev new))))
