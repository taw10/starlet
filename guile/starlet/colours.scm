(define-module (starlet colours)
  #:use-module (oop goops)
  #:export (<colour>
             make-colour-cmy
             make-colour-rgb
             colour-as-cmy
             white))


(define-class <colour> (<object>)
  (type
    #:init-form (error "Colour type must be specified")
    #:init-keyword #:type
    #:getter colour-type)

  (value
    #:init-form (error "Colour value must be specified")
    #:init-keyword #:value
    #:getter colour-value))


(define-method (write (col <colour>) port)
  (format port "#<<colour> ~a ~a>"
          (colour-type col)
          (colour-value col)))


(define (make-colour-cmy c m y)
  (make <colour>
        #:type 'cmy
        #:value (list c m y)))


(define (make-colour-rgb r g b)
  (make <colour>
        #:type 'rgb
        #:value (list r g b)))


(define white
  (make-colour-cmy 0 0 0))


(define (colour-as-cmy col)
  (colour-value col))
