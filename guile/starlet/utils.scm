(define-module (starlet utils)
  #:export (return-unspecified
            print-hash-table))


(define (return-unspecified)
  (if #f 1))

(define (print-hash-table ht)
  (hash-for-each (lambda (key value)
                   (display key)
                   (display " ---> ")
                   (display value)
                   (newline))
                 ht))
