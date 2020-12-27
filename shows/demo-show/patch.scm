(define-module (demo-show patch)
  #:use-module (starlet base)
  #:use-module (starlet fixture-library generic)
  #:use-module (starlet fixture-library robe))


;; Conventional dimmers
(define-public dim1 (patch-fixture! <generic-dimmer> 1))
(define-public dim2 (patch-fixture! <generic-dimmer> 2))
(define-public dim3 (patch-fixture! <generic-dimmer> 3))
(define-public dim4 (patch-fixture! <generic-dimmer> 4))
(define-public dim5 (patch-fixture! <generic-dimmer> 5))
(define-public dim6 (patch-fixture! <generic-dimmer> 6))
(define-public dim7 (patch-fixture! <generic-dimmer> 7))
(define-public dim8 (patch-fixture! <generic-dimmer> 8))
(define-public dim9 (patch-fixture! <generic-dimmer> 9))
(define-public dim10 (patch-fixture! <generic-dimmer> 10))
(define-public dim11 (patch-fixture! <generic-dimmer> 11))
(define-public dim12 (patch-fixture! <generic-dimmer> 12))


;; Some moving lights
(define-public mh1 (patch-fixture! <robe-dl7s-mode1> 59))
(define-public mh2 (patch-fixture! <robe-dl7s-mode1> 146))


;; A generic LED fixture
(define-public ledpar (patch-fixture! (generic-rgb '(r g b 0 intensity FL)) 204))
