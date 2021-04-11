(define-module (starlet colours)
  #:use-module (oop goops)
  #:use-module (ice-9 exceptions)
  #:export (<colour>
             colour?
             make-colour-cmy
             make-colour-rgb
             colour-as-cmy
             colour-as-rgb
             interpolate-colour
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


(define (colour? c)
  (is-a? c <colour>))


(define cyan car)
(define magenta cadr)
(define yellow caddr)

(define red car)
(define green cadr)
(define blue caddr)

(define-method (display (col <colour>) port)
  (format port "#<<colour> ~a ~a>"
          (colour-type col)
          (colour-value col)))


(define-method (write (col <colour>) port)
  (let ((cmy (colour-as-cmy col)))
    (format port "(make-colour-cmy ~a ~a ~a)"
            (cyan cmy)
            (magenta cmy)
            (yellow cmy))))


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


(define (colour-as-rgb col)
  (let ((val (colour-value col)))
    (case (colour-type col)

      ((rgb)
       val)

      ((cmy)
       (list (- 100 (red val))
             (- 100 (green val))
             (- 100 (blue val))))

      (else
        (raise-exception (make-exception
                           (make-exception-with-message "Unrecognised colour type")
                           (make-exception-with-irritants (colour-type col))))))))


(define (colour-as-cmy col)
  (let ((val (colour-value col)))
    (case (colour-type col)

      ((cmy)
       val)

      ((rgb)
       (list (- 100 (red val))
             (- 100 (green val))
             (- 100 (blue val))))

      (else
        (raise-exception (make-exception
                           (make-exception-with-message "Unrecognised colour type")
                           (make-exception-with-irritants (colour-type col))))))))


(define (interpolate-cmy a b frac)
  (let ((cmy1 (colour-as-cmy a))
        (cmy2 (colour-as-cmy b)))
    (make-colour-cmy
      (+ (cyan cmy1) (* frac (- (cyan cmy2) (cyan cmy1))))
      (+ (magenta cmy1) (* frac (- (magenta cmy2) (magenta cmy1))))
      (+ (yellow cmy1) (* frac (- (yellow cmy2) (yellow cmy1)))))))


(define* (interpolate-colour a b frac #:key (interpolation-type 'linear-cmy))
  (case interpolation-type

    ((linear-cmy)
     (interpolate-cmy a b frac))

    (else
      (raise-exception (make-exception
                         (make-exception-with-message
                           "Unrecognised colour interpolation type")
                         (make-exception-with-irritants interpolation-type))))))
