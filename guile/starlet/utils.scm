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
  #:export (print-hash-table
            copy-hash-table
            in-range
            mean
            flatten-sublists
            more-than-one
            hirestime))


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

  (define (listify a)
    (if (list? a)
        a
        (list a)))

  (fold (lambda (a prev)
          (append prev (listify a)))
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

