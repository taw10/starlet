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

(osc-send x1k2 "/x1k2/leds/*" 'off)

(osc-playback-indicators pb x1k2 "/x1k2/leds/101" "/x1k2/leds/29" "/x1k2/leds/25")
(osc-playback-controls pb osc-server "/x1k2/buttons/101" "/x1k2/buttons/29" "/x1k2/buttons/25")
(osc-playback-indicators pb x1k2 "/x1k2/leds/102" "/x1k2/leds/32" "/x1k2/leds/28")
(osc-playback-controls pb osc-server "/x1k2/buttons/102" "/x1k2/buttons/32" "/x1k2/buttons/28")

(add-osc-method osc-server "/x1k2/buttons/30" "" (lambda ()
                                                   (reload-cue-list! pb)
                                                   (reassert-current-cue! pb)))
(osc-send x1k2 "/x1k2/leds/30" 'green)

(add-osc-method osc-server "/x1k2/buttons/31" "" sel)
(osc-send x1k2 "/x1k2/leds/31" 'green)

(osc-select-button osc-server "/x1k2/buttons/17" x1k2 "/x1k2/leds/17" front-leds)
(osc-select-button osc-server "/x1k2/buttons/18" x1k2 "/x1k2/leds/18" front-wash)
(osc-select-button osc-server "/x1k2/buttons/21" x1k2 "/x1k2/leds/21" mhLL)
(osc-select-button osc-server "/x1k2/buttons/22" x1k2 "/x1k2/leds/22" mhL)
(osc-select-button osc-server "/x1k2/buttons/23" x1k2 "/x1k2/leds/23" mhR)
(osc-select-button osc-server "/x1k2/buttons/24" x1k2 "/x1k2/leds/24" mhRR)

(osc-parameter-encoder osc-server "/x1k2/encoders/1" x1k2 "/x1k2/leds/1" pan)
(osc-parameter-encoder osc-server "/x1k2/encoders/2" x1k2 "/x1k2/leds/2" tilt)
(osc-parameter-encoder osc-server "/x1k2/encoders/3" x1k2 "/x1k2/leds/3" gobo)
(osc-parameter-encoder osc-server "/x1k2/encoders/102" x1k2 "/x1k2/nothing" intensity)

(osc-state-fader osc-server "/x1k2/faders/4"
                 (lighting-state
                   (at mhL mhR colour (rgb 40 20 70))
                   (at mhL mhR 100)
                   (at front-wash 100)
                   (at domeL domeR 100)))
