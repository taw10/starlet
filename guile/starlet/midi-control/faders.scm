;;
;; starlet/midi-control/faders.scm
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
(define-module (starlet midi-control faders)
  #:use-module (starlet midi-control base)
  #:use-module (starlet state)
  #:use-module (starlet fixture)
  #:use-module (starlet colours)
  #:use-module (starlet scanout)
  #:use-module (starlet utils)
  #:use-module (starlet attributes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
  #:export (set-midi-control-map!
             fader
             jogwheel
             state-on-fader))


(define-class <parameter-controller> (<object>)
  (callbacks
    #:init-keyword #:callbacks
    #:getter get-callbacks
    #:setter set-callbacks!)

  (control-map
    #:init-keyword #:control-map
    #:getter get-control-map))


(define-record-type <fader-spec>
  (make-fader cc attr-name congruent incongruent)
  fader-spec?
  (cc           fader-cc-number)
  (attr-name    fader-attr-name)
  (congruent    fader-congruent-note)
  (incongruent  fader-incongruent-note))


(define-record-type <jogwheel-spec>
  (make-jogwheel cc attr-name active-note)
  jogwheel-spec?
  (cc           jogwheel-cc-number)
  (attr-name    jogwheel-attr-name)
  (active-note  jogwheel-active-note))


(define* (fader cc
                attr-name
                #:key
                (congruent #f)
                (incongruent #f))
  (make-fader cc attr-name congruent incongruent))


(define* (jogwheel cc
                   attr-name
                   #:key
                   (active #f))
  (make-jogwheel cc attr-name active))


(define (name-for-fader-state controller cc-number)
  (call-with-output-string
    (lambda (port)
      (format port "faderstate-~a-cc~a"
              controller
              cc-number))))


(define* (state-on-fader controller
                         cc-number
                         state)
  (register-state!
    (lighting-state
      (state-for-each
        (lambda (fix attr val)
          (at fix attr
              (lambda ()

                (let ((cc-val (get-cc-value controller cc-number)))

                  ;; Fader position known?
                  (if cc-val

                      (if (intensity? attr)

                          ;; Intensity parameters get scaled according to the fader
                          (* 0.01 val (ccval->percent cc-val))

                          ;; Non-intensity parameters just get set in our new state,
                          ;; but only if the fader is up!
                          (if (> cc-val 0)
                              val
                              'no-value))

                      ;; Fader position unknown
                      'no-value)))))

        state))
    #:unique-name (name-for-fader-state controller cc-number)))


(define (current-values fixture-list attr-name)
  (map (lambda (fix)
         (current-value fix attr-name))
       fixture-list))


(define (fixtures-with-attr fixture-list attr-name)
  (let ((attrs (map (cut find-attr <> attr-name) fixture-list)))
    (fold (lambda (fix attr old)
            (if attr
              (cons (cons fix (car old))
                    (cons attr (cdr old)))
              old))
          (cons '() '())
          fixture-list attrs)))


(define (clamp-to-attr-range attr-obj val)
  (let ((r (get-attr-range-maybe-colour attr-obj)))
    (max (car r)
         (min (cadr r)
              val))))


(define (attr-scale controller attr)
  (let ((sens-level (get-controller-sensitivity controller)))
    (cond
      ((= sens-level 1) 0.02)
      ((= sens-level 2) 0.1)
      ((= sens-level 3) 0.5)
      ((= sens-level 4) 1.5)
      ((= sens-level 5) 3.0))))


(define* (at-midi-jogwheel controller
                           fixture-list
                           attr
                           cc-number
                           #:key (led #f))

  (define (ccval->offset controller a)
    (if (eq? a 127)
        (- (attr-scale controller attr))
        (attr-scale controller attr)))

  (let ((fixtures (car (fixtures-with-attr fixture-list attr))))
    (unless (null? fixtures)

      (when led
        (send-note-on controller led))

      (let ((old-vals (current-values fixtures attr))
            (offset 0))
        (register-midi-cc-callback!
          controller
          #:cc-number cc-number
          #:func (lambda (prev-cc-val new-cc-value)
                   (set! offset (+ offset (ccval->offset controller
                                                         new-cc-value)))
                   (for-each (lambda (fix old-val)
                               (let ((attr-obj (find-attr fix attr)))
                                 (when (and attr-obj
                                            (continuous-attribute? attr-obj))
                                   (set-in-state! programmer-state
                                                  fix
                                                  attr
                                                  (clamp-to-attr-range
                                                    attr-obj
                                                    (+ old-val offset))
                                                  controller))))
                             fixtures old-vals)))))))


(define (get-attr-range-maybe-colour attr-obj)
  (if (colour-attribute? attr-obj)
      '(0 100)
      (get-attr-range attr-obj)))


(define (fader-congruent vals attrs)
  (mean (map (lambda (val attr)
               (scale-to-range val
                               (get-attr-range-maybe-colour attr)
                               '(0 127)))
             vals attrs)))


(define (fader-up-gradients initial-vals
                            attrs
                            congruent-val)
  (map (lambda (initial-val attr)
         (let ((attr-max (cadr (get-attr-range-maybe-colour attr))))
           (if (< congruent-val 127)
               (/ (- attr-max initial-val)
                  (- 127 congruent-val))
               0)))
       initial-vals
       attrs))


(define (fader-down-gradients initial-vals
                              attrs
                              congruent-val)
  (map (lambda (initial-val attr)
         (let ((attr-min (car (get-attr-range-maybe-colour attr))))
           (if (> congruent-val 0)
               (/ (- initial-val attr-min)
                  congruent-val)
               0)))

       initial-vals
       attrs))


(define (apply-fader cc-offset
                     attr-name
                     gradients
                     initial-vals
                     fixtures
                     controller)
  (for-each (lambda (fix initial-val gradient)
              (when (colour-component-id? attr-name)
                (set-in-state!
                  programmer-state
                  fix
                  'colour
                  (current-value fix 'colour)
                  controller))
              (set-in-state! programmer-state
                             fix
                             attr-name
                             (+ initial-val
                                (* gradient cc-offset))
                             controller))
            fixtures
            initial-vals
            gradients))


(define* (at-midi-fader controller
                        fixture-list
                        attr-name
                        cc-number
                        #:key
                        (led-incongruent #f)
                        (led #f))

  (let ((fixtures-attrs (fixtures-with-attr fixture-list attr-name)))
    (unless (null? (car fixtures-attrs))
      (let* ((fixtures (car fixtures-attrs))
             (attrs (cdr fixtures-attrs))
             (initial-vals (current-values fixtures attr-name))
             (congruent-val (fader-congruent initial-vals attrs))
             (up-gradients (fader-up-gradients initial-vals attrs congruent-val))
             (dn-gradients (fader-down-gradients initial-vals attrs congruent-val))
             (cc-val (get-cc-value controller cc-number))
             (congruent (and cc-val (= cc-val congruent-val))))

        (if congruent
            (send-note-on controller led)
            (send-note-on controller led-incongruent))

        (register-midi-cc-callback!
          controller
          #:cc-number cc-number
          #:func (lambda (prev-cc-val new-cc-value)

                   (if congruent

                       (cond
                         ((> new-cc-value congruent-val)
                          (apply-fader (- new-cc-value congruent-val)
                                       attr-name
                                       up-gradients
                                       initial-vals
                                       fixtures
                                       controller))
                         ((<= new-cc-value congruent-val)
                          (apply-fader (- new-cc-value congruent-val)
                                       attr-name
                                       dn-gradients
                                       initial-vals
                                       fixtures
                                       controller)))

                       (when (or (and (not prev-cc-val)
                                      (= new-cc-value congruent-val))
                                 (and prev-cc-val new-cc-value
                                      (in-range congruent-val
                                                prev-cc-val
                                                new-cc-value)))
                         (set! congruent #t)
                         (send-note-on controller led)))))))))


(define (midi-control-attr controller control-spec fixture-list)
  (cond

   ((jogwheel-spec? control-spec)
    (at-midi-jogwheel controller
                      fixture-list
                      (jogwheel-attr-name control-spec)
                      (jogwheel-cc-number control-spec)
                      #:led (jogwheel-active-note control-spec)))

   ((fader-spec? control-spec)
    (at-midi-fader controller
                   fixture-list
                   (fader-attr-name control-spec)
                   (fader-cc-number control-spec)
                   #:led (fader-congruent-note control-spec)
                   #:led-incongruent (fader-incongruent-note control-spec)))))


(define (led-off controller leds)
  (cond
    ((list? leds)
     (for-each (lambda (note)
                 (send-note-off controller note))
               leds))
    ((number? leds)
     (send-note-off controller leds))))


(define (scrub-parameter-controller! controller parameter-controller)

  ;; Remove all the old callbacks
  (for-each (lambda (callback)
              (remove-midi-callback! controller callback))
            (get-callbacks parameter-controller))

  ;; Switch off all the old LEDs
  (for-each
    (lambda (cs)
      (cond
        ((jogwheel-spec? cs)
         (led-off controller (jogwheel-active-note cs)))
        ((fader-spec? cs)
         (led-off controller (fader-congruent-note cs))
         (led-off controller (fader-incongruent-note cs)))))
    (get-control-map parameter-controller)))


(define (update-midi-controls controller fixture-list)

  (scrub-parameter-controller! controller
                               (get-parameter-controller controller))

  (set-callbacks!
    (get-parameter-controller controller)
    (map (lambda (control-spec)
           (midi-control-attr controller control-spec fixture-list))
         (get-control-map (get-parameter-controller controller)))))


(define (set-midi-control-map! controller . new-control-map)
  (when controller
    (let ((old-parameter-controller (get-parameter-controller controller)))

      ;; Remove the old parameter controller
      (when old-parameter-controller
        (scrub-parameter-controller! controller old-parameter-controller))

      (set-parameter-controller!
        controller
        (make <parameter-controller>
              #:callbacks '()
              #:control-map new-control-map))

      ;; If this is the first time, add the callbacks
      (unless old-parameter-controller

        ;; Selection changed
        (add-hook!
          selection-hook
          (lambda (fixture-list)
            (update-midi-controls controller fixture-list)))

        ;; Value changed
        (add-update-hook! programmer-state
                          (lambda (source)
                            (unless (eq? source controller)
                              (update-midi-controls controller (get-selection))))))

      ;; If there is a selection, run the callback now
      (let ((current-selection (get-selection)))
        (when current-selection
          (update-midi-controls controller current-selection))))))
