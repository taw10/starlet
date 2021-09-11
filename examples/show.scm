(use-modules
  (starlet fixture)
  (starlet state)
  (starlet playback)
  (starlet scanout)
  (starlet effects)
  (starlet colours)
  (starlet clock)
  (starlet fixture-library generic dimmer)
  (starlet fixture-library generic rgb)
  (starlet fixture-library robe mmxspot mode1)
  (starlet fixture-library robe mmxwashbeam mode1)
  (starlet midi-control base)
  (starlet midi-control button-utils)
  (starlet midi-control faders))

;; Start MIDI control
(define controller (make-midi-controller "/dev/snd/midiC0D0" 14))

;; Fixtures are normal GOOPS objects, fixture types are GOOPS classes
(patch-fixture! led <generic-rgb> 1 #:universe 4)

(patch-many! red <generic-dimmer> '(9 13 20 24))


;; Multiple fixtures can be defined at once
;; Example: foh1, foh2, foh3, ... foh8 on channels 1-8 of universe 0
(patch-many! foh <generic-dimmer> (iota 8 1))


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

;; Fixtures can be grouped together
(define ltruss (list ltruss1 ltruss2 ltruss3 ltruss4 ltruss5 ltruss6))
(define rtruss (list rtruss1 rtruss2 rtruss3 rtruss4 rtruss5 rtruss6))
(define floor (list floor1 floor2 floor3 floor4 floor5 floor6))


(define my-state
  (lighting-state (at ltruss6 (quote zoom) 5300/127)
                  (at ltruss6 (quote intensity) 100)
                  (at ltruss6 (quote colour) (make-colour-cmy 3500/127 0 0))
                  (at ltruss6 (quote pan) 306)
                  (at ltruss6 (quote tilt) 120)
                  (at rtruss1 (quote pan) 223)
                  (at rtruss1 (quote zoom) 5100/127)
                  (at rtruss1 (quote tilt) 120)
                  (at rtruss1 (quote colour) (make-colour-cmy 0 4100/127 3400/127))
                  (at rtruss1 (quote intensity) 100)))


;; Put a lighting state on a MIDI fader
(state-on-fader controller 19 my-state)


(set-midi-control-map!
  controller
  (list
    (list 'intensity                     'fader    16 '(108 72))
    (list 'pan                           'jogwheel 0  124)
    (list 'tilt                          'jogwheel 1  125)
    (list (colour-component-id 'cyan)    'fader    4  '(120 84))
    (list (colour-component-id 'magenta) 'fader    5  '(121 85))
    (list (colour-component-id 'yellow)  'fader    6  '(122 86))
    (list 'cto                           'fader    7  '(123 87))
    (list 'iris                          'fader    8  '(116 80))
    (list 'zoom                          'fader    9  '(117 81))
    (list 'focus                         'fader    10 '(118 82))))


(define pb
  (make-playback
    #:cue-list-file "examples/show.qlist.scm"))


;; Set up MIDI controller buttons to run cues
(make-go-button controller pb 12
                #:ready-note 20
                #:pause-note 16)
(make-stop-button controller pb 24
                  #:ready-note 24)
(make-back-button controller pb 28
                  #:ready-note 28)

;; A second set of go/stop buttons, because this works well on my controller
(make-go-button controller pb 15
                #:ready-note 23
                #:pause-note 19)
(make-stop-button controller pb 27
                  #:ready-note 27)
(make-back-button controller pb 31
                  #:ready-note 31)


;; Set up some buttons for quick access to fixtures
(select-on-button controller 32 ltruss
                  #:ready-note 68)
(select-on-button controller 33 rtruss
                  #:ready-note 69)
(select-on-button controller 34 foh
                  #:ready-note 70)
(select-on-button controller 35 floor
                  #:ready-note 71)


;; Red button de-selects everything
(select-on-button controller 26 #f
                  #:ready-note 26)

(cut-to-cue-number! pb 0)
