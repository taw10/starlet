(cue-list

    (cue 1
         (lighting-state
           (at ltruss1 (quote pan) 206)
           (at ltruss1 (quote tilt) 108.0)
           (at ltruss1 (quote zoom) 6300/127)
           (at ltruss1 (quote intensity) 80)
           (at ltruss1 (quote colour) (make-colour-cmy 0 600/127 3800/127))

           (at rtruss6 (quote pan) 334)
           (at rtruss6 (quote intensity) 80)
           (at rtruss6 (quote zoom) 4200/127)
           (at rtruss6 (quote tilt) 111)
           (at rtruss6 (quote colour) (make-colour-cmy 0 100/127 3100/127))

           (at red (quote intensity) 30)

           (at led 'intensity 20)
           (at led 'colour (make-colour-cmy 50 21 0))))

    (cue 1.5
         (lighting-state
           (at red 'intensity 50)))

    (cue 2
         (lighting-state
           (apply-state my-state))
         #:up-time 1
         #:down-time 1)

    (cue 2.5
         (lighting-state
           (apply-state my-state)
           (at ltruss6 'colour (make-colour-cmy 100 0 0))
           (at rtruss1 'colour (make-colour-cmy 0 80 0))

           (at led 'intensity 100)
           (at led 'colour (make-colour-cmy 50 100 0)))
         #:up-time 3
         #:down-time 3
         #:attr-time 3

         (cue-part (led) #:up-time 1 #:up-delay 4))

    (cue 3
         (lighting-state
           (at floor3 (quote pan) 299)
           (at floor3 (quote intensity) 156)
           (at floor3 (quote tilt) 48)
           (at floor4 'colour (make-colour-cmy 200/127 11500/127 100))
           (at floor3 'colour (make-colour-cmy 200/127 11500/127 100))
           (at floor4 (quote intensity) 127)
           (at floor4 (quote pan) 239)
           (at floor4 (quote tilt) 49)
           (at led 'colour (make-colour-rgb 0 50 80))
           (at led 'intensity 100))
         #:up-time 3
         #:down-time 3))
