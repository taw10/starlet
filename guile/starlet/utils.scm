(define-module (starlet utils)
  #:export (return-unspecified
            print-hash-table
            copy-hash-table))


(define (return-unspecified)
  (if #f 1))

(define (print-hash-table ht)
  (hash-for-each (lambda (key value)
                   (display key)
                   (display " ---> ")
                   (display value)
                   (newline))
                 ht))

(define (copy-hash-table ht)
  (let ((new-ht (make-hash-table)))
    (hash-for-each (lambda (key value)
                     (hash-set! new-ht key value))
                   ht)
    new-ht))
