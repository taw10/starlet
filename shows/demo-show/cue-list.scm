(define-module (demo-show cue-list)
  #:use-module (starlet base)
  #:use-module (starlet playback)
  #:use-module (starlet effects)
  #:use-module (demo-show patch))


(define example-state-1
  (lighting-state

   ;; Front wash
   (at dim1 'intensity 50)
   (at dim2 'intensity 50)
   (at dim3 'intensity 50)

   ;; Sidelight
   (at dim7 'intensity (flash 5))
   (at dim8 'intensity 50)))



(define example-state-2
  (lighting-state

   ;; Front wash
   (at dim1 'intensity 10)
   (at dim2 'intensity 99)
   (at dim3 'intensity 35)

   ;; Sidelight
   (at dim7 'intensity (flash 9))))



(define-public my-cue-list
  (cue-list

   (cue 1
        (cue-state (apply-state example-state-1)
                   (at mh1 'pan 40)
                   (at mh1 'tilt 32)
                   (at mh1 'yellow 30))
        #:up-time 3
        #:down-time 5
        #:attr-time 0
        #:attr-delay 1.5)

   (cue 2
        (cue-state (apply-state example-state-2)
                   (at dim12 40)
                   (at mh1 'pan 20)
                   (at mh1 'tilt 12)
                   (at mh1 'yellow 80))
        #:up-time 3
        #:down-time 3
        #:attr-time 0
        #:attr-delay 1.5

        (cue-part ((list mh1 'pan 'tilt)
                   dim11)
                  #:up-time 0.5
                  #:down-time 0.5
                  #:attr-time 0.5))


   (cue 2.2
        (cue-state (at dim6 50)
                   (at mh2 'tilt 20))
        #:track-intensities #t
        #:attr-time 0)

   (cue 2.5
        (cue-state (at dim1 'intensity 100)
                   (at dim9 100))
        #:track-intensities #t
        #:up-time 1
        #:down-time 1
        #:attr-time 0)

   (cue 3
        (cue-state (blackout (current-state)))
        #:up-time 0
        #:down-time 2

        (cue-part (dim1
                   dim2
                   (cons dim3 'intensity))
                  #:down-time 2
                  #:down-delay 1))))
