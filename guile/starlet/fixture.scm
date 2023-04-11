;;
;; starlet/fixture.scm
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
(define-module (starlet fixture)
  #:use-module (starlet colours)
  #:use-module (starlet utils)
  #:use-module (starlet attributes)
  #:use-module (oop goops)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:export (<fixture>
            get-fixture-name
            get-fixture-addr
            get-fixture-universe
            get-fixture-attrs
            find-attr
            fixture?
            scanout-fixture

            attr-continuous
            attr-list
            attr-colour
            define-fixture

            get-attr-type
            get-attr-range
            get-attr-home-val
            continuous-attribute?
            colour-attribute?))


(define-class <fixture-attribute> (<object>)
  (name
    #:init-form (error "Attribute name must be specified")
    #:init-keyword #:name
    #:getter get-attr-name)

  (range
    #:init-value '()
    #:init-keyword #:range
    #:getter get-attr-range)

  (type
    #:init-value 'continuous
    #:init-keyword #:type
    #:getter get-attr-type)

  (home-value
    #:init-value 0
    #:init-keyword #:home-value
    #:getter attr-home-value)

  (comment
    #:init-value ""
    #:init-keyword #:comment
    #:getter attr-comment))


(define-class <fixture> (<object>)
  (name
    #:init-form (error "Fixture name must be specified")
    #:init-keyword #:name
    #:getter get-fixture-name)

  (universe
    #:init-value #f
    #:init-keyword #:uni
    #:getter get-fixture-universe
    #:setter set-fixture-universe!)

  (start-addr
    #:init-value #f
    #:init-keyword #:sa
    #:getter get-fixture-addr
    #:setter set-fixture-addr!)

  (friendly-name
    #:init-value "Fixture"
    #:init-keyword #:friendly-name
    #:getter get-fixture-friendly-name
    #:setter set-fixture-friendly-name!)

  (scanout-func
    #:init-value (lambda (universe start-addr value set-dmx) #f)
    #:init-keyword #:scanout-func
    #:getter get-scanout-func))


(define-syntax attr-continuous
  (syntax-rules ()
    ((_ attr-name attr-range attr-home-value comment)
     (make <fixture-attribute>
       #:name attr-name
       #:range attr-range
       #:type 'continuous
       #:home-value attr-home-value
       #:comment comment))
    ((_ attr-name attr-range attr-home-value)
     (make <fixture-attribute>
       #:name attr-name
       #:range attr-range
       #:type 'continuous
       #:home-value attr-home-value))))


(define-syntax attr-list
  (syntax-rules ()
    ((_ attr-name attr-allowed-values attr-home-value)
     (make <fixture-attribute>
       #:name attr-name
       #:range attr-allowed-values
       #:type 'list
       #:home-value attr-home-value))
    ((_ attr-name attr-allowed-values attr-home-value comment)
     (make <fixture-attribute>
       #:name attr-name
       #:range attr-allowed-values
       #:type 'list
       #:home-value attr-home-value
       #:comment comment))))


(define-syntax attr-colour
  (syntax-rules ()
    ((_ attr-name attr-home-value comment)
     (make <fixture-attribute>
       #:name attr-name
       #:type 'colour
       #:home-value attr-home-value
       #:comment comment))
    ((_ attr-name attr-home-value)
     (make <fixture-attribute>
       #:name attr-name
       #:type 'colour
       #:home-value attr-home-value))))


(define-generic scanout-fixture)


(define (get-fixture-attrs fix)
  (slot-ref fix 'attributes))


(define (fixture? f)
  (is-a? f <fixture>))


(define-method (find-attr (fix <fixture>) (attr-name <starlet-attribute>))
  (find (lambda (a)
          (eq? (get-attr-name a)
               attr-name))
        (get-fixture-attrs fix)))


(define-method (find-attr (fix <fixture>) (attr-name <colour-component-id>))
  (find-attr fix colour))


(define-method (find-attr fix attr-name)
  (raise-exception
    (make-exception
      (make-exception-with-message "Invalid parameters")
      (make-exception-with-irritants fix))))


(define-method (get-attr-home-val (fix <fixture>) (attr <starlet-attribute>))
  (let ((attr-obj (find-attr fix attr)))
    (if attr-obj
        (attr-home-value attr-obj)
        'fixture-does-not-have-attribute)))


(define-method (get-attr-home-val (fix <fixture>) (attr <colour-component-id>))
  (extract-colour-component
    (get-attr-home-val fix colour)
    attr))


(define (continuous-attribute? aobj)
  (eq? 'continuous
       (get-attr-type aobj)))


(define (colour-attribute? aobj)
  (eq? 'colour
       (get-attr-type aobj)))


(define-syntax define-fixture
  (syntax-rules (fixture-attributes)

    ((_ classname
        (fixture-attributes attr ...)
        scanout-code ...)

     (begin
       (define-class classname (<fixture>)
         (attributes #:init-form (list attr ...)))
       (define-method (scanout-fixture (fixture classname))
         scanout-code ...)))))
