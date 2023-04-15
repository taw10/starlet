;;
;; starlet/osc/engine.scm
;;
;; Copyright © 2023 Thomas White <taw@bitwiz.org.uk>
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
(define-module (starlet osc engine)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (start-osc
            define-osc-method))


;; Find the library
(define liblo (dynamic-link "liblo"))


;; Get the API routines we'll need
(define lo_server_thread_new
  (foreign-library-function liblo "lo_server_thread_new"
                            #:return-type '*
                            #:arg-types (list '* '*)))

(define lo_server_thread_start
  (foreign-library-function liblo "lo_server_thread_start"
                            #:return-type int
                            #:arg-types (list '*)))

(define lo_server_thread_add_method
  (foreign-library-function liblo "lo_server_thread_add_method"
                            #:return-type '*
                            #:arg-types (list '* '* '* '* '*)))


;; High-level API
(define error-callback
  (procedure->pointer void
                      (lambda (num msg path)
                        (format #t "OSC Error: ~a ~a\n"
                                num (pointer->string msg)))
                      (list int '* '*)))


(define (start-osc)
  (let ((server (lo_server_thread_new (string->pointer "7770")
                                      %null-pointer)))
    (lo_server_thread_start server)
    server))


(define (wrap-method-callback cb)
  (procedure->pointer int
                      (lambda (path types argv argc data user-data)
                        0)
                      (list '* '* '* int '* '*)))


(define (define-osc-method server-thread address callback)
  (lo_server_thread_add_method server-thread
                               (string->pointer address)
                               (string->pointer "")
                               (wrap-method-callback callback)
                               %null-pointer))
