#!/usr/bin/guile
!#

(define useful-links

  (list

    '((Starlet)     (https://github.com/taw10/starlet))

    '((Conjure)   (https://conjure.fun/))

    '((Video: Conversational Software Development by Oliver Caldwell)
      (https://www.youtube.com/watch?v=RU28xy9JXxs))

    '((Open Lighting Project)   (https://www.openlighting.org/))

    '((ChamSys MagicVis) (https://chamsyslighting.com/products/magicq))))


;; Very basic Conjure demo
(+ 1 1)
(define a 5)
(* a 3)
(reverse '(a b c d e f))


;; Basic setup
(use-modules
  (starlet base)
  (starlet playback)
  (starlet effects)
  (starlet colours)
  (starlet fixture-library generic)
  (starlet fixture-library robe)
  (starlet midi-control base)
  (starlet midi-control button-utils)
  (starlet midi-control faders))

;; Start readout to OLA
(start-ola-output)

;; Start MIDI control
(start-midi-control "/dev/snd/midiC0D0"
            #:channel 14)

;; Fixtures are normal GOOPS objects, fixture types are GOOPS classes
(patch-fixture! foh1 <generic-dimmer> 8)
(patch-fixture! foh2 <generic-dimmer> 7)
(patch-fixture! foh3 <generic-dimmer> 6)
(patch-fixture! foh4 <generic-dimmer> 5)
(patch-fixture! foh5 <generic-dimmer> 4)
(patch-fixture! foh6 <generic-dimmer> 3)
(patch-fixture! foh7 <generic-dimmer> 2)
(patch-fixture! foh8 <generic-dimmer> 1)

(patch-fixture! red1 <generic-dimmer> 9)
(patch-fixture! red2 <generic-dimmer> 13)
(patch-fixture! red3 <generic-dimmer> 20)
(patch-fixture! red4 <generic-dimmer> 24)

(patch-fixture! ltruss1 <robe-mmxspot-mode1> 1 #:universe 1)
(patch-fixture! ltruss2 <robe-mmxspot-mode1> 39 #:universe 1)
(patch-fixture! ltruss3 <robe-mmxspot-mode1> 77 #:universe 1)
(patch-fixture! ltruss4 <robe-mmxspot-mode1> 115 #:universe 1)
(patch-fixture! ltruss5 <robe-mmxspot-mode1> 153 #:universe 1)
(patch-fixture! ltruss6 <robe-mmxspot-mode1> 191 #:universe 1)

(patch-fixture! rtruss1 <robe-mmxspot-mode1> 229 #:universe 1)
(patch-fixture! rtruss2 <robe-mmxspot-mode1> 267 #:universe 1)
(patch-fixture! rtruss3 <robe-mmxspot-mode1> 305 #:universe 1)
(patch-fixture! rtruss4 <robe-mmxspot-mode1> 343 #:universe 1)
(patch-fixture! rtruss5 <robe-mmxspot-mode1> 381 #:universe 1)
(patch-fixture! rtruss6 <robe-mmxspot-mode1> 419 #:universe 1)

(patch-fixture! floor1 <robe-mmxwashbeam-mode1> 100)
(patch-fixture! floor2 <robe-mmxwashbeam-mode1> 134)
(patch-fixture! floor3 <robe-mmxwashbeam-mode1> 168)
(patch-fixture! floor4 <robe-mmxwashbeam-mode1> 202)
(patch-fixture! floor5 <robe-mmxwashbeam-mode1> 236)
(patch-fixture! floor6 <robe-mmxwashbeam-mode1> 270)


;; Set a parameter
(at foh1 'intensity 80)
(at floor4 'intensity 100)
(at floor4 'colour (make-colour-rgb 0 100 0))


;; If parameter is not specified, 'intensity' is understood
(at red1 100)


;; Functions can be assigned to parameters
(at foh1 'intensity (lambda (time)
                      (* 50
                         (+ 1 (sin (* 2 time))))))


;; Effects library
(at floor2 'intensity 100)
(at floor2 'colour (make-colour-cmy 0 0 100))
(at floor2 'pan 0)
(at floor2 'tilt (sinewave 0.5 100 170))


;; Clean up
(clear-state! programmer-state)


;; Fixtures can be grouped together
(define rtruss (list rtruss1
                     rtruss2
                     rtruss3
                     rtruss4
                     rtruss5
                     rtruss6))

(define floor (list floor1
                    floor2
                    floor3
                    floor4
                    floor5
                    floor6))

(define red (list red1
                  red2
                  red3
                  red4))

(define foh (list foh1
                  foh2
                  foh3
                  foh4
                  foh5
                  foh6
                  foh7
                  foh8))

(at red 100)
(at rtruss 100)
(at rtruss 'zoom 80)
(at rtruss 'colour (make-colour-cmy 0 100 0))
(at rtruss 'tilt 70)
(at rtruss 'pan 200)
(at rtruss 'prism #f)


;; Clean up again
(clear-state! programmer-state)


;; Control fixtures via MIDI surface
(sel ltruss6)
(at 100)        ;; Without fixture name 'at' applies to selected fixture(s)
(sel rtruss1)
(sel #f)
(sel ltruss5)
(at 'colour (make-colour-cmy 0 0 100))


;; Record a state to a variable, then clear up
(define my-state
  (state-source programmer-state))

(clear-state! programmer-state)


;; Recall a stored state
(apply-state my-state)

(clear-state! programmer-state)


;; Put a lighting state on a MIDI fader
(state-on-fader 19 my-state)

;; Create a cue list
(define my-cue-list
  (cue-list

    (cue 1
         (lighting-state
           (at ltruss1 (quote pan) 206)
           (at ltruss1 (quote tilt) 108.0)
           (at ltruss1 (quote zoom) 6300/127)
           (at ltruss1 (quote intensity) 109)
           (at ltruss1 (quote colour) (make-colour-cmy 0 600/127 3800/127))

           (at rtruss6 (quote pan) 334)
           (at rtruss6 (quote intensity) 133)
           (at rtruss6 (quote zoom) 4200/127)
           (at rtruss6 (quote tilt) 111)
           (at rtruss6 (quote colour) (make-colour-cmy 0 100/127 3100/127))

           (at red4 (quote intensity) 30)
           (at red3 (quote intensity) 30)
           (at red1 (quote intensity) 30)
           (at red2 (quote intensity) 30)))

    (cue 2
         (lighting-state
           (apply-state my-state))
         #:up-time 1
         #:down-time 1)

    (cue 2.5
         (lighting-state
           (apply-state my-state)
           (at ltruss6 'colour (make-colour-cmy 100 0 0))
           (at rtruss1 'colour (make-colour-cmy 0 40 0)))
         #:up-time 3
         #:down-time 3
         #:attr-time 2)

    (cue 3
         (lighting-state
           (at floor3 (quote pan) 299)
           (at floor3 (quote intensity) 156)
           (at floor3 (quote tilt) 48)
           (at floor4 'colour (make-colour-cmy 200/127 11500/127 100))
           (at floor3 'colour (make-colour-cmy 200/127 11500/127 100))
           (at floor4 (quote intensity) 127)
           (at floor4 (quote pan) 239)
           (at floor2 (quote intensity) -58)
           (at floor4 (quote tilt) 49))
         #:up-time 3
         #:down-time 3)))


(define pb
  (make-playback my-cue-list))

(cut-to-cue-number! pb 0)


;; Run cues
(go! pb)


;: Jump between cues
(cut-to-cue-number! pb 1)


;; Run cues out of order
(run-cue-number! pb 2)
