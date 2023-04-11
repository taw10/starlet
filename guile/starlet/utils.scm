;;
;; starlet/utils.scm
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
(define-module (starlet utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 control)
  #:export (print-hash-table
            copy-hash-table
            in-range
            mean
            flatten-sublists
            more-than-one
            hirestime
            lsb
            msb
            ensure-number
            round-dmx
            scale-to-range
            scale-and-clamp-to-range
            percent->dmxval8
            percent->dmxval16
            comment
            hash-table-empty?
            lookup))


(define (print-hash-table ht)
  (hash-for-each (lambda (key value)
                   (display key)
                   (display " ---> ")
                   (display value)
                   (newline))
                 ht))

(define (copy-hash-table ht)
  (let ((new-ht (make-hash-table)))
    (hash-for-each (lambda (key value)
                     (hash-set! new-ht key value))
                   ht)
    new-ht))



(define (in-range a val1 val2)
  (or
   (and (>= a val1)
        (<= a val2))
   (and (>= a val2)
        (<= a val1))))


(define (mean vals)
  (/ (fold + 0 vals)
     (length vals)))


(define (flatten-sublists l)
  (fold
    (lambda (el prev)
      (if (list? el)
        (append (flatten-sublists el) prev)
        (cons el prev)))
    '() l))


(define (more-than-one a)
  (if (nil? a)
      #f
      (not (nil? (cdr a)))))


(define (hirestime)
  (let ((a (gettimeofday)))
    (+ (car a)
       (/ (cdr a)
          1000000))))


(define (msb val)
  (round-dmx (euclidean-quotient val 256)))

(define (lsb val)
  (round-dmx (euclidean-remainder val 256)))


(define (round-dmx a)
  (inexact->exact
   (min 255 (max 0 (round a)))))


(define (ensure-number value irritating)
  (unless (number? value)
    (raise-exception (make-exception
                       (make-exception-with-message "Value is not a number")
                       (make-exception-with-irritants irritating)))))


(define (percent->dmxval8 val)
  (round-dmx
   (scale-to-range val '(0 100) '(0 255))))


(define (percent->dmxval16 val)
  (scale-to-range val '(0 100) '(0 65535)))


(define (scale-to-range val orig-range dest-range)

  (define (range r)
    (- (cadr r) (car r)))

  (+ (car dest-range)
     (* (range dest-range)
        (/ (- val (car orig-range))
           (range orig-range)))))


(define (clamp-to-range val val1 val2)
  (let ((minval (min val1 val2))
        (maxval (max val1 val2)))
  (max minval
       (min val maxval))))


;; Like scale-to-range, but result is clamped within dest-range
(define (scale-and-clamp-to-range val orig-range dest-range)
  (clamp-to-range
    (scale-to-range val orig-range dest-range)
    (car dest-range)
    (cadr dest-range)))


(define-syntax comment
  (syntax-rules ()
    ((_ body ...)
     #f)))


(define (hash-table-empty? ht)
  (let/ec
    return
    (hash-for-each-handle
      (lambda (key)
        (return #f))
      ht)
    #t))


(define (lookup key dictionary)
  (cond
    ((nil? dictionary)
     #f)
    ((eq? key (caar dictionary))
     (cadr (car dictionary)))
    (else
      (lookup key (cdr dictionary)))))
