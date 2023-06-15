;;
;; starlet/state.scm
;;
;; Copyright © 2020-2022 Thomas White <taw@bitwiz.org.uk>
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
  #:use-module (starlet utils)
  #:use-module (starlet attributes)
  #:use-module (starlet selection)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (<starlet-state>
            make-empty-state
            lighting-state?
            get-state-name
            state-for-each
            state-map->list
            state-map
            copy-state
            clear-state!
            print-state
            state-source
            set-in-state!
            state-find
            current-state
            at
            apply-state
            combine-states
            show-state
            lighting-state
            programmer-state
            ps
            home-fixture!
            blackout
            blackout!
            value->number
            atomically-overlay-state!
            update-state!
            add-update-hook!
            state-empty?
            remove-fixtures-from-state!
            remove-fixture-from-state!
            remove-selection-from-programmer!))


;; A "state" is an atomically-updating container for an immutable
;; hash table mapping (fixture-object . attribute-name-object) pairs to values
;; which can be numbers, symbols, colours, boolean values and more
;; depending on the type of attribute.  Values can also be
;; functions which provide the value.
(define-class <starlet-state> (<object>)
  (hash-table-box
    #:init-form (make-atomic-box (make-hash-table))
    #:getter get-ht-box)
  (update-hook
    #:init-form (make-hook 1)
    #:getter get-update-hook))


(define (lighting-state? a)
  (is-a? a <starlet-state>))


;; The state used to build a new scene for recording
(define programmer-state (make <starlet-state>))
(define ps programmer-state)


(define (add-update-hook! state proc)
  (add-hook! (get-update-hook state)
             proc))


(define (find-colour state fix)
  (let ((col (state-find fix colour state)))
    (if (eq? 'no-value col)

        (let ((home-col (get-attr-home-val fix colour)))
          (if (eq? 'fixture-does-not-have-attribute home-col)
              (raise-exception (make-exception
                                 (make-exception-with-message
                                   "Fixture doesn't have colour attribute")
                                 (make-exception-with-irritants fix)))
              home-col))

        col)))


(define-method (update-state! (state <starlet-state>))
  ;; Basic state object needs no updates
  #f)


(define-method (set-in-state! (state <starlet-state>)
                              (fix <fixture>)
                              (attr <starlet-attribute>)
                              value
                              source)
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
      (set-in-state! state fix attr))  ;; Try again

    (run-hook (get-update-hook state) source)))


(define-method (set-in-state! (state <starlet-state>)
                              (fix <fixture>)
                              (attr <starlet-attribute>)
                              value)
  (set-in-state! state fix attr value #f))


;; Set any intensity attributes in the current state to zero
(define (blackout!)
  (let ((state (current-state)))
    (state-for-each
      (lambda (fix attr val)
        (when (intensity? attr)
          (set-in-state! state fix attr 0.0)))
      state)))


;; Set all attributes of the fixture to their home values
(define (home-fixture! fix)
  (for-each (lambda (attr)
              (at fix attr (get-attr-home-val fix attr)))
            (get-fixture-attrs fix)))


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


(define blackout
  (make-empty-state))


(define (state-for-each func state)
  (hash-for-each (lambda (key value)
                   (func (car key)
                         (cdr key)
                         value))
                 (atomic-box-ref (get-ht-box state))))


(define (state-find fix attr state)
  (hash-ref (atomic-box-ref (get-ht-box state))
            (cons fix attr)
            'no-value))


(define (state-map->list func state)
  (hash-map->list (lambda (key value)
                    (func (car key)
                          (cdr key)
                          value))
                  (atomic-box-ref (get-ht-box state))))


(define (state-map func state)
  (let ((out-state (make-empty-state)))
    (hash-for-each
      (lambda (key value)
        (set-in-state!
          out-state
          (car key)
          (cdr key)
          (func (car key)
                (cdr key)
                value)))
      (atomic-box-ref (get-ht-box state)))
    out-state))


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


(define (combine-states a b)
  (lighting-state
    (apply-state a)
    (apply-state b)))


(define (print-state a)
  (pretty-print (state-source a)))

(define-method (write (st <starlet-state>) port)
  (write
    (state-source st)
    port))


(define (clamp-to-attr-range fix attr val)
  (if (number? val)
    (let ((attr-obj (find-attr fix attr)))
      (if (continuous-attribute? attr-obj)
        (let ((range (get-attr-range attr-obj)))
          (max (car range)
               (min (cadr range) val)))
        val))
    val))


(define (quote-if-symbol a)
  (if (symbol? a)
    (list 'quote a)
    a))


(define (state-source a)
  (cons 'lighting-state
        (state-map->list (lambda (fix attr val)
                           (list 'at
                                 (get-fixture-name fix)
                                 (canonical-name attr)
                                 (quote-if-symbol
                                   (clamp-to-attr-range fix attr val))))
                         a)))


;; Coerce something from a state object into a number for scanout
(define (value->number val)
  (if (procedure? val)
      (value->number (val))
      val))


(define (clear-state! state)
  (let* ((old-ht (atomic-box-ref (get-ht-box state)))
         (new-ht (make-hash-table)))
    (unless (eq? (atomic-box-compare-and-swap!
                   (get-ht-box state)
                   old-ht
                   new-ht)
                 old-ht)
      (clear-state! state))) ;; Try again

  (run-hook (get-update-hook state) #f))


(define (partition3 pred1 pred2 input)
  (receive (output1 others)
           (partition pred1 input)
           (receive (output2 others)
                    (partition pred2 others)
                    (values output1 output2 others))))


(define (set-fixtures fixtures attribute value)
  (for-each
    (lambda (fix)
      (if (fixture-has-attr? fix attribute)
        (set-in-state! (current-state)
                       fix
                       attribute
                       (clamp-to-attr-range fix attribute value))
        (error "Fixture does not have attribute"
               (get-fixture-name fix)
               (canonical-name attribute))))
    fixtures))


;; (at <fixtures/groups> [<attribute>] <level> [<attribute> <level>...])
;; (at fix1 100)                      <-- Set intensity of single fixture
;; (at fix1 intensity 100)           <-- Explicit attribute name
;; (at fix1 fix2 100)                 <-- Multiple fixtures
;; (at fix1 fix2 pan 36)             <-- Multiple fixtures + explicit attribute
;; (at group1 fix1 intensity 100)    <-- Groups can be used instead of fixtures
;; (at fix1 100 pan 36)              <-- Set multiple attributes
;; NB Can't set multiple fixtures and attributes: (at fix1 pan 35 fix2 tilt 22)

(define (at . args)
  (let ((selection (get-selection)))
    (receive (fixtures attribute value)
           (partition3 fixture? attribute? (flatten-sublists args))
           (cond

             ((nil? value)
              (error "at: Value not specified"))

             ((or (more-than-one value)
                  (more-than-one attribute))
              (error "at: Only one attribute or value name"))

             ((and (nil? fixtures)
                   (nil? attribute))
              (if (nil? selection)
                  'no-fixtures-selected
                  (set-fixtures selection intensity (car value))))

             ((nil? attribute)
              (set-fixtures fixtures intensity (car value)))

             ((nil? fixtures)
              (if (nil? selection)
                  'no-fixtures-selected
                  (set-fixtures selection (car attribute) (car value))))

             (else
               (set-fixtures fixtures (car attribute) (car value)))))))


(define (state-empty? st)
  (hash-table-empty?
    (atomic-box-ref
      (get-ht-box st))))


(define (remove-fixtures-from-state! st fixture-list)
  (let ((new-ht (make-hash-table))
        (old-ht (atomic-box-ref (get-ht-box st))))
    (state-for-each
      (lambda (fix attr val)
        (unless (memq fix fixture-list)
          (hash-set! new-ht (cons fix attr) val)))
      st)
    (if (eq? old-ht (atomic-box-compare-and-swap!
                      (get-ht-box st)
                      old-ht
                      new-ht))
      (run-hook (get-update-hook st) #f)
      (remove-fixtures-from-state! st fixture-list))))


(define (remove-fixture-from-state! st fix)
  (remove-fixtures-from-state! st (list fix)))


(define (remove-selection-from-programmer!)
  (remove-fixtures-from-state!
    programmer-state
    (get-selection)))
