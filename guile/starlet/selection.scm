;;
;; starlet/selection.scm
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
(define-module (starlet selection)
  #:use-module (starlet utils)
  #:use-module (starlet fixture)
  #:use-module (srfi srfi-1)
  #:export (sel
             add-sel
             toggle-sel
             desel
             selection-hook
             get-selection
             get-selection-as-string
             selected?))


(define selection-hook (make-hook 1))

(define selection '())


(define (get-selection)
  selection)


(define (dotted-fixture-name s)
  (with-output-to-string
    (lambda ()
      (format #t "~a.~a" (second s) (third s)))))


(define (get-selection-as-string)
  (cat-with-spaces
    (map
      (lambda (s)
        (let ((name (get-fixture-name s)))
          (if (symbol? name)
            (symbol->string name)
            (dotted-fixture-name name))))
      selection)))


(define (sel . fixture-list)
  (if (nil? fixture-list)
      (set! selection '())
      (if (not (car fixture-list))
          (set! selection '())
          (set! selection (flatten-sublists fixture-list))))
  (run-hook selection-hook selection))


(define (toggle-sel . fixture-list)
  (if (selected? fixture-list)
    (desel fixture-list)
    (add-sel fixture-list)))


(define (add-sel . fixture-list)
  (set! selection
    (append selection
            (filter (lambda (fix)
                      (not (selected? fix)))
                    (flatten-sublists fixture-list))))
  (run-hook selection-hook selection))


(define (selected? . fixture-list)
  (every (lambda (fix)
           (memq fix selection))
         (flatten-sublists fixture-list)))


(define (desel . fixture-list)
  (let ((remove-us (flatten-sublists fixture-list)))
    (set! selection
      (filter (lambda (fix)
                (not (memq fix remove-us)))
              selection)))
  (run-hook selection-hook selection))
