;;
;; starlet/state.scm
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
(define-module (starlet state)
  #:use-module (starlet fixture)
  #:use-module (starlet colours)
  #:use-module (starlet utils)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:export (<starlet-state>
            make-empty-state
            get-state-name
            state-for-each
            state-map
            clear-state!
            print-state
            state-source
            set-in-state!
            state-find
            current-state
            at
            apply-state
            show-state
            lighting-state
            programmer-state
            blackout
            sel
            selection-hook
            value->number
            atomically-overlay-state!))


;; A "state" is an atomically-updating container for an immutable
;; hash table mapping (fixture-object . attribute-symbol) pairs to values
;; which can be numbers, symbols, colours, boolean values and more
;; depending on the type of attribute.  Values can also be
;; functions which provide the value.
(define-class <starlet-state> (<object>)
  (hash-table-box
    #:init-form (make-atomic-box (make-hash-table))
    #:getter get-ht-box))


;; The state used to build a new scene for recording
(define programmer-state (make <starlet-state>))


(define (find-colour state fix)
  (let ((col (state-find fix 'colour state)))
    (if (eq? 'no-value col)

        (let ((home-col (get-attr-home-val fix 'colour)))
          (if (eq? 'fixture-does-not-have-attribute home-col)
              (raise-exception (make-exception
                                 (make-exception-with-message
                                   "Fixture doesn't have colour attribute")
                                 (make-exception-with-irritants fix)))
              home-col))

        col)))


(define-method (set-in-state! (state <starlet-state>)
                              (fix <fixture>)
                              (attr <colour-component-id>)
                              new-val)
  (let ((current-colour (find-colour state fix))
        (colour-component (get-colour-component attr)))

    (cond

      ((eq? colour-component 'cyan)
       (let ((orig-colour (colour-as-cmy current-colour)))
         (set-in-state! state fix 'colour
                        (make-colour-cmy new-val
                                         (magenta orig-colour)
                                         (yellow orig-colour)))))

      ((eq? colour-component 'magenta)
       (let ((orig-colour (colour-as-cmy current-colour)))
         (set-in-state! state fix 'colour
                        (make-colour-cmy (cyan orig-colour)
                                         new-val
                                         (yellow orig-colour)))))

      ((eq? colour-component 'yellow)
       (let ((orig-colour (colour-as-cmy current-colour)))
         (set-in-state! state fix 'colour
                        (make-colour-cmy (cyan orig-colour)
                                         (magenta orig-colour)
                                         new-val))))

      ((eq? colour-component 'red)
       (let ((orig-colour (colour-as-rgb current-colour)))
         (set-in-state! state fix 'colour
                        (make-colour-rgb new-val
                                         (green orig-colour)
                                         (blue orig-colour)))))

      ((eq? colour-component 'green)
       (let ((orig-colour (colour-as-rgb current-colour)))
         (set-in-state! state fix 'colour
                        (make-colour-rgb (red orig-colour)
                                         new-val
                                         (blue orig-colour)))))

      ((eq? colour-component 'blue)
       (let ((orig-colour (colour-as-rgb current-colour)))
         (set-in-state! state fix 'colour
                        (make-colour-rgb (red orig-colour)
                                         (green orig-colour)
                                         new-val)))))))


(define-method (set-in-state! (state <starlet-state>)
                              (fix <fixture>)
                              (attr <symbol>)
                              value)
  (let* ((old-ht (atomic-box-ref (get-ht-box state)))
         (new-ht (copy-hash-table old-ht)))
    (hash-set! new-ht
               (cons fix attr)
               value)
    (unless (eq? (atomic-box-compare-and-swap!
                   (get-ht-box state)
                   old-ht
                   new-ht)
                 old-ht)
      (set-in-state! state fix attr))))  ;; Try again


(define (blackout state)
  (state-for-each
    (lambda (fix attr val)
      (when (intensity? attr)
        (set-in-state! state fix attr 0.0)))
    state))


;; Set a single attribute to home position
(define (home-attr! state fix attr)
  (set-in-state! state
                 fix
                 attr
                 (get-attr-home-val fix attr)))


(define (copy-state state)
  (let ((new-state (make-empty-state)))
    (state-for-each (lambda (fix attr val)
                      (set-in-state! new-state
                                     fix
                                     attr
                                     val))
                    state)
    new-state))


(define (make-empty-state)
  (make <starlet-state>))


(define (state-for-each func state)
  (hash-for-each (lambda (key value)
                   (func (car key)
                         (cdr key)
                         value))
                 (atomic-box-ref (get-ht-box state))))


(define-method (state-find (fix <fixture>)
                           (attr <symbol>)
                           (state <starlet-state>))
  (hash-ref (atomic-box-ref (get-ht-box state))
            (cons fix attr)
            'no-value))


(define-method (state-find (fix <fixture>)
                           (attr <colour-component-id>)
                           (state <starlet-state>))
  (let ((col (state-find fix 'colour state)))
    (if (eq? 'no-value col)
        'no-value
        (extract-colour-component col attr))))


(define (state-map func state)
  (hash-map->list (lambda (key value)
                    (func (car key)
                          (cdr key)
                          value))
                  (atomic-box-ref (get-ht-box state))))


(define (apply-state state)
  "Apply the contents of 'state' to the current state, on top of the \
pre-existing contents."
  (state-for-each at state))


(define (show-state state)
  "Clear the current state, and apply the contents of 'state'"
  (clear-state! (current-state))
  (state-for-each at state))


(define (atomically-overlay-state! state newbits)
  "Apply 'newbits' within 'state', as a single atomic operation."
  (let* ((old-ht (atomic-box-ref (get-ht-box state)))
         (new-ht (copy-hash-table old-ht)))
    (state-for-each (lambda (fix attr val)
                      (hash-set! new-ht
                                 (cons fix attr)
                                 val))
                    newbits)
    (unless (eq? (atomic-box-compare-and-swap!
                   (get-ht-box state)
                   old-ht
                   new-ht)
                 old-ht)
      (atomically-overlay-state! state newbits))))  ;; Try again


(define current-state (make-parameter programmer-state))


(define-syntax lighting-state
  (syntax-rules ()
    ((_ body ...)
     (parameterize ((current-state (make-empty-state)))
       body ...
       (current-state)))))


(define (print-state a)
  (pretty-print (state-source a)))


(define (state-source a)
  (cons 'lighting-state
        (state-map (lambda (fix attr val)
                     (list 'at
                           (get-fixture-name fix)
                           (list 'quote attr)
                           val))
                   a)))


;; Coerce something from a state object into a number for scanout
(define (value->number val time)
  (if (procedure? val)
      (value->number (val time) time)
      val))


(define (clear-state! state)
  (let* ((old-ht (atomic-box-ref (get-ht-box state)))
         (new-ht (make-hash-table)))
    (unless (eq? (atomic-box-compare-and-swap!
                   (get-ht-box state)
                   old-ht
                   new-ht)
                 old-ht)
      (clear-state! state))))  ;; Try again


(define (partition3 pred1 pred2 input)
  (receive (output1 others)
           (partition pred1 input)
           (receive (output2 others)
                    (partition pred2 others)
                    (values output1 output2 others))))


(define (set-fixtures fixtures attr-name value)
  (for-each (lambda (fix)
              (set-in-state! (current-state)
                             fix
                             (car attr-name)
                             (car value)))
            fixtures))


;; (at <fixtures/groups> [<attribute>] <level> [<attribute> <level>...])
;; (at fix1 100)                      <-- Set intensity of single fixture
;; (at fix1 'intensity 100)           <-- Explicit attribute name
;; (at fix1 fix2 100)                 <-- Multiple fixtures
;; (at fix1 fix2 'pan 36)             <-- Multiple fixtures + explicit attribute
;; (at group1 fix1 'intensity 100)    <-- Groups can be used instead of fixtures
;; (at fix1 100 'pan 36)              <-- Set multiple attributes
;; NB Can't set multiple fixtures and attributes: (at fix1 'pan 35 fix2 'tilt 22)

(define (at . args)
  (receive (fixtures attr-name value)
           (partition3 fixture? symbol? (flatten-sublists args))
           (cond

             ((nil? value)
              (error "at: Value not specified"))

             ((or (more-than-one value)
                  (more-than-one attr-name))
              (error "at: Only one attribute or value name"))

             ((and (nil? fixtures)
                   (nil? attr-name))
              (if (nil? selection)
                  'no-fixtures-selected
                  (set-fixtures selection '(intensity) value)))

             ((nil? attr-name)
              (set-fixtures fixtures '(intensity) value))

             ((nil? fixtures)
              (if (nil? selection)
                  'no-fixtures-selected
                  (set-fixtures selection attr-name value)))

             (else
               (set-fixtures fixtures attr-name value)))))


(define selection-hook (make-hook 1))

(define selection '())


(define (sel . fixture-list)
  (if (nil? fixture-list)
      (set! selection '())
      (if (not (car fixture-list))
          (set! selection '())
          (set! selection (flatten-sublists fixture-list))))
  (run-hook selection-hook selection))

