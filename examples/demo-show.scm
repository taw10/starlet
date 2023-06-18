(use-modules
  (starlet fixture)
  (starlet state)
  (starlet playback)
  (starlet engine)
  (starlet scanout)
  (starlet effects)
  (starlet colours)
  (starlet clock)
  (starlet attributes)
  (starlet cue-list)
  (starlet attributes)
  (starlet selection)
  (starlet fixture-library generic dimmer)
  (starlet fixture-library stairville z120m)
  (starlet fixture-library robe dl7s)
  (open-sound-control server-thread)
  (open-sound-control client)
  (starlet open-sound-control utils))


;; Patch fixtures
(patch-fixture! mhLL <robe-dl7s-mode1> 1)
(patch-fixture! mhL <robe-dl7s-mode1> 52)
(patch-fixture! mhR <robe-dl7s-mode1> 104)
(patch-fixture! mhRR <robe-dl7s-mode1> 156)
(patch-fixture! washL <generic-dimmer> 260)
(patch-fixture! washM <generic-dimmer> 261)
(patch-fixture! washR <generic-dimmer> 262)
(patch-fixture! ledLL <stairville-z120m-6ch> 238)
(patch-fixture! ledL <stairville-z120m-6ch> 232)
(patch-fixture! ledR <stairville-z120m-6ch> 250)
(patch-fixture! ledRR <stairville-z120m-6ch> 244)
(patch-fixture! goboL <generic-dimmer> 263)
(patch-fixture! goboR <generic-dimmer> 264)
(patch-fixture! domeL <generic-dimmer> 265)
(patch-fixture! domeR <generic-dimmer> 266)
(patch-fixture! apronL <generic-dimmer> 267)
(patch-fixture! apronR <generic-dimmer> 268)
(patch-fixture! highsideL <generic-dimmer> 269)
(patch-fixture! highsideR <generic-dimmer> 270)
(patch-fixture! floodL <generic-dimmer> 271)
(patch-fixture! floodR <generic-dimmer> 272)


;; Set up some groups
(define front-leds (list ledLL ledL ledR ledRR))
(define front-wash (list washL washM washR))


;; Make a cue list
(define my-cues
  (cue-list

    (cue 1
      (crossfade 3
        (lighting-state
          (at washL washM washR 80))))

    (cue 2
      (crossfade 2 5
        (lighting-state
          (at washL washM washR 0)
          (at ledL ledR colour (cmy 0 0 24))
          (at ledL ledR 100))))

    (cue 3
      (snap blackout))

    (cue 4
      (crossfade 1
        (lighting-state
          (at washM 100))))

    (cue 5
      track-intensities
      (crossfade 5
        (lighting-state
          (at ledL ledR 30)))
      (crossfade 2 #:up-delay 5
        (lighting-state
          (at apronL apronR 100))))

    (cue 6
      (snap
        (lighting-state
          (at washL washR 20))))))


(define pb
  (make-playback
    #:cue-list my-cues
    #:recovery-file "recovery.q"))


;; OSC controls
(define osc-server (make-osc-server-thread "osc.udp://:7770"))
(define x1k2 (make-osc-address "osc.udp://localhost:7771"))

(send-selection-updates-to (make-osc-address "osc.udp://localhost:7772"))

(osc-playback-controls pb osc-server x1k2 "/x1k2/buttons/LAYER" "/x1k2/buttons/M" "/x1k2/buttons/I")
(osc-playback-controls pb osc-server x1k2 "/x1k2/buttons/SHIFT" "/x1k2/buttons/P" "/x1k2/buttons/L")

(osc-send x1k2 "/x1k2/buttons/N/set-led" 'green)
(add-osc-method osc-server "/x1k2/buttons/N/press" "" (lambda ()
                                                        (reload-cue-list! pb)
                                                        (reassert-current-cue! pb)))

(osc-send x1k2 "/x1k2/buttons/O/set-led" 'green)
(add-osc-method osc-server "/x1k2/buttons/O/press" "" sel)

(osc-select-button front-leds osc-server x1k2 "/x1k2/buttons/A")
(osc-select-button front-wash osc-server x1k2 "/x1k2/buttons/B")
(osc-select-button mhLL osc-server x1k2 "/x1k2/buttons/E")
(osc-select-button mhL osc-server x1k2 "/x1k2/buttons/F")
(osc-select-button mhR osc-server x1k2 "/x1k2/buttons/G")
(osc-select-button mhRR osc-server x1k2 "/x1k2/buttons/H")

(osc-parameter-encoder pan osc-server x1k2 "/x1k2/encoders/1")
(osc-parameter-encoder tilt osc-server x1k2 "/x1k2/encoders/2")
(osc-parameter-encoder gobo osc-server x1k2 "/x1k2/encoders/3")
(osc-parameter-encoder intensity osc-server x1k2 "/x1k2/encoders/6")
(osc-smart-potentiometer color-temperature osc-server x1k2 "/x1k2/potentiometers/4")

(osc-state-fader osc-server x1k2 "/x1k2/faders/4"
                 (lighting-state
                   (at mhL mhR colour (rgb 40 20 70))
                   (at mhL mhR 100)
                   (at front-wash 100)
                   (at domeL domeR 100)))
