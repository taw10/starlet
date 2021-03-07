(define-module (starlet effects)
  #:use-module (starlet base)
  #:export (flash
             sinewave))


(define pi (* 2 (acos 0)))

(define (square-wave time hz)
  (if (> (sin (* 2 pi hz time))
         0)
      100
      0))

(define (flash hz)
  (lambda (time)
    (square-wave time hz)))


(define (sinewave hz range-min range-max)
  (lambda (time)
    (+ range-min
       (* (/ (- range-max range-min) 2)
          (+ 1 (sin (* 2 pi hz time)))))))
