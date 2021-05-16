;;
;; starlet/clocks.scm
;;
;; Copyright © 2021 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet clock)
  #:use-module (oop goops)
  #:use-module (ice-9 exceptions)
  #:export (<starlet-clock>
            make-clock
            stop-clock!
            start-clock!
            reverse-clock!

            <starlet-delayed-clock>
            make-delayed-clock

            clock-stopped?
            clock-reversed?
            elapsed-fraction))


(define (time-now)
  (let ((a (gettimeofday)))
    (+ (car a)
       (/ (cdr a)
          1000000))))


;; "Real" clocks are straightforward objects for measuring differences in time
;; between now and some point in the past, allowing for temporarily "pausing"
;; the clock.  The time difference cannot be negative: if the start time is in
;; the future, then "time-elapsed" will return 0.

(define-class <starlet-clock> (<object>)
  (start-real-time
    #:init-form (time-now)
    #:init-keyword #:start-time
    #:getter get-start-real-time
    #:setter set-start-real-time!)

  (start-elapsed-time
    #:init-value 0
    #:getter get-start-elapsed-time
    #:setter set-start-elapsed-time!)

  (stopped
    #:init-value #f
    #:getter clock-stopped?
    #:setter set-clock-stopped!)

  (reversed
    #:init-value #f
    #:getter clock-reversed?
    #:setter set-clock-reversed!))


(define (make-clock)
  (make <starlet-clock>))


(define-method (time-elapsed (clock <starlet-clock>))
  (if (clock-stopped? clock)
      (get-start-elapsed-time clock)
      (max 0
           (if (clock-reversed? clock)

               (- (get-start-elapsed-time clock)
                  (- (time-now)
                     (get-start-real-time clock)))

               (+ (get-start-elapsed-time clock)
                  (- (time-now)
                     (get-start-real-time clock)))))))


;; Stop the clock running
(define-method (stop-clock! (clock <starlet-clock>))
  (set-start-elapsed-time! clock (time-elapsed clock))
  (set-clock-stopped! clock #t))


;; Start the clock running (forwards)
(define-method (start-clock! (clock <starlet-clock>))
  (set-start-elapsed-time! clock (time-elapsed clock))
  (set-start-real-time! clock (time-now))
  (set-clock-reversed! clock #f)
  (set-clock-stopped! clock #f))


;; Start the clock running, backwards
(define-method (reverse-clock! (clock <starlet-clock>))
  (set-start-elapsed-time! clock (time-elapsed clock))
  (set-start-real-time! clock (time-now))
  (set-clock-reversed! clock #t)
  (set-clock-stopped! clock #f))


;; Delayed clocks refer to a parent clock for anything to do with "real" time.
;; Note, however, that the parent clock can be another delayed clock.

(define-class <starlet-delayed-clock> (<object>)
  (parent
    #:init-form (error "Parent clock must be specified")
    #:init-keyword #:parent
    #:getter get-parent-clock)

  (delay-time
    #:init-form (error "Delay time must be specified")
    #:init-keyword #:delay-time
    #:getter get-delay-time)

  (duration
    #:init-value #f
    #:init-keyword #:duration
    #:getter get-duration))


(define-method (clock-stopped? (clock <starlet-delayed-clock>))
  (clock-stopped? (get-parent-clock clock)))


(define-method (clock-reversed? (clock <starlet-delayed-clock>))
  (clock-reversed? (get-parent-clock clock)))


(define-method (time-elapsed (clock <starlet-delayed-clock>))
  (max 0 (- (time-elapsed (get-parent-clock clock))
            (get-delay-time clock))))


(define-method (elapsed-fraction (clock <starlet-delayed-clock>))
  (if (= (get-duration clock) 0)
      (if (> (time-elapsed clock) 0)
          1.0
          0.0)
      (min 1.0
           (/ (time-elapsed clock)
              (get-duration clock)))))


(define-method (stop-clock! (clock <starlet-delayed-clock>))
  (error "Can only stop a top-level clock."))


(define-method (start-clock! (clock <starlet-delayed-clock>))
  (error "Can only start a top-level clock."))


(define-method (reverse-clock! (clock <starlet-delayed-clock>))
  (error "Can only reverse a top-level clock."))


(define (make-delayed-clock clock delay-time duration)
  (make <starlet-delayed-clock>
        #:parent clock
        #:delay-time delay-time
        #:duration duration))
