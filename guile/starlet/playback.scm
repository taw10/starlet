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
            cut-to-cue-number!
            get-playback-cue-number
            run-cue-number!
            go!
            cut!
            stop!
            back!
            reload-cue-list!
            reassert-current-cue!
            state-change-hook
            playback-state))


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

  (recovery-file
    #:init-keyword #:recovery-file
    #:getter get-playback-recovery-file)

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


(define (get-playback-cue-number pb)
  (let ((cue-idx (get-next-cue-index pb)))
    (if cue-idx
      (cue-index-to-number (get-playback-cue-list pb)
                           (max 0 (- cue-idx 1)))
      #f)))


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


(define (read-recovery-file! pb)
  (with-exception-handler
    (lambda (exn)
      (display "Failed to read recovery file - going to cue zero\n")
      (cut-to-cue-index! pb 0))
    (lambda ()
      (call-with-input-file
        (get-playback-recovery-file pb)
        (lambda (port)
          (let ((val (read port)))
            (if (number? val)
              (cut-to-cue-number! pb val)
              (cut-to-cue-index! pb 0))))))
    #:unwind? #t))


(define (write-recovery-file! pb the-cue-number)
  (with-exception-handler
    (lambda (exn)
      (display "Failed to write recovery file (just FYI)\n")
      (display exn))
    (lambda ()
      (call-with-output-file
        (get-playback-recovery-file pb)
        (lambda (port)
          (write (qnum the-cue-number) port))))
    #:unwind? #t))


(define* (make-playback #:key
                        (cue-list-file #f)
                        (cue-list #f)
                        (recovery-file #f))
  (let ((new-playback (make <starlet-playback>
                            #:cue-list (if cue-list-file
                                         (read-cue-list-file cue-list-file)
                                         cue-list)
                            #:cue-list-file cue-list-file
                            #:recovery-file recovery-file)))
    (register-state! new-playback)
    (if recovery-file
      (read-recovery-file! new-playback)
      (cut-to-cue-index! new-playback 0))
    new-playback))


(define (set-playback-state! pb state)
  (atomic-box-set! (state-box  pb) state)
  (run-hook (state-change-hook pb) state))


(define (cut-to-cue-index! pb cue-index)
  (clear-state! pb)
  (set-next-cue-index! pb (+ cue-index 1))
  (set-pb-cue-clock! pb #f)
  (set-running-cue! pb #f)
  (set-playback-state! pb 'ready)

  (let ((the-cue (vector-ref (get-playback-cue-list pb)
                             cue-index)))
    ;; Set the actual state
    (for-each
      (lambda (part)
        (state-for-each
          (lambda (fix attr val)
            (set-in-state! pb fix attr (lambda () val)))
          (get-cue-part-state part)))
      (get-cue-parts the-cue))

    ;; Set the preset state on top
    (state-for-each
      (lambda (fix attr val)
        (set-in-state! pb fix attr (lambda () val)))
      (get-preset-state the-cue))

    (write-recovery-file! pb (get-cue-number the-cue))))


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
  (let ((nci (get-next-cue-index pb)))
    (if nci
      (if (< nci (vector-length (get-playback-cue-list pb)))
        (cut-to-cue-index! pb (get-next-cue-index pb))
        'no-more-cues-in-list)
      'next-cue-unspecified)))


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


(define (blank-everything state)
  (state-map
    (lambda (fix attr val)
      (if (intensity? attr)
        0.0
        'no-value))
    state))


(define (run-cue-index! pb cue-index)
  (let* ((the-cue (vector-ref (get-playback-cue-list pb) cue-index))
         (overlay-state (make-empty-state))
         (cue-clock (get-cue-clock the-cue))
         (fade-time 0))

    ;; Start by fading the previous contents of the playback down, using the
    ;; "main" transition effect
    (receive
      (overlay-part transition-time)
      ((transition-func (get-cue-part-transition
                          (car (get-cue-parts the-cue))))
       (blank-everything pb)
       pb
       cue-clock)
      (atomically-overlay-state!
        overlay-state
        overlay-part)
      (set! fade-time transition-time))

    ;; Stack all the cue parts on top
    (for-each
      (lambda (part)
        (receive
          (overlay-part transition-time)
          ((transition-func (get-cue-part-transition part))
           (get-cue-part-state part)
           pb
           cue-clock)
          (atomically-overlay-state!
            overlay-state
            overlay-part)
          (set! fade-time (max fade-time transition-time))))
      (get-cue-parts the-cue))

    (set-clock-expiration-time! cue-clock fade-time)
    (atomically-overlay-state! pb overlay-state)
    (set-pb-cue-clock! pb cue-clock)
    (set-running-cue! pb the-cue)
    (reset-clock! cue-clock)
    (start-clock! cue-clock)
    (set-playback-state! pb 'running)
    (write-recovery-file! pb (get-cue-number the-cue))))


(define-method (num-cues (pb <starlet-playback>))
  (num-cues (get-playback-cue-list pb)))


(define-method (update-state! (pb <starlet-playback>))
  (when (and (get-pb-cue-clock pb)
             (clock-expired? (get-pb-cue-clock pb))
             (eq? 'running (atomic-box-ref (state-box pb))))
    (when (eq? 'running (atomic-box-compare-and-swap! (state-box pb)
                                                      'running
                                                      'ready))
      (run-hook (state-change-hook pb) 'ready)

      ;; Pre-set fixtures
      (let ((st (get-preset-state (get-running-cue pb))))
        (state-for-each
          (lambda (fix attr val)
            (set-in-state! pb fix attr (lambda () val)))
          st))

      (set-running-cue! pb #f))))


(define (next-cue-number pb)
  (let ((next-cue-index (get-next-cue-index pb))
        (the-cue-list (get-playback-cue-list pb)))
    (if (< next-cue-index (vector-length the-cue-list))
      (exact->inexact
        (cue-index-to-number
          the-cue-list
          next-cue-index))
      'no-more-cues-in-list)))


(define (playback-state pb)
  (atomic-box-ref (state-box pb)))


(define-method (write (pb <starlet-playback>) port)
  (let ((cur-cue (get-playback-cue-number pb)))
    (format port
            "#<<starlet-playback> state: ~a current-cue: ~a next-cue: ~a>"
            (playback-state pb)
            (if cur-cue
              (exact->inexact cur-cue)
              'current-cue-unspecified)
            (if cur-cue
              (next-cue-number pb)
              'next-cue-unspecified))))


(define (reassert-current-cue! pb)
  (let ((cur-cue (get-playback-cue-number pb)))
    (if cur-cue
      (cut-to-cue-number! pb cur-cue)
      'current-cue-unspecified)))
