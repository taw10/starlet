(define-module (starlet state-rw)
  #:use-module (starlet base)
  #:export (write-state))


(define (write-state state)
  (state-map
    (lambda (fix attr val)
      (list 'at
            (get-fixture-name fix)
            (get-attr-name attr)
            val))
    state))
