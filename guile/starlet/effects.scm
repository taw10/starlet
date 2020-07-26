(define-module (starlet effects)
  #:use-module (starlet base)
  #:export (flash))


(define pi (* 2 (acos 0)))

(define (square-wave time hz)
  (if (> (sin (* 2 pi hz time))
         0)
      100
      0))

(define (flash hz)
  (lambda (time)
    (square-wave time hz)))
