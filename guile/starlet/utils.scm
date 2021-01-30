(define-module (starlet utils)
  #:use-module (srfi srfi-1)
  #:export (return-unspecified
            print-hash-table
            copy-hash-table
            partial
            in-range
            mean
            flatten-sublists
            more-than-one))


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


(define (partial f second-val)
  (lambda (first-val)
    (f first-val second-val)))


(define (in-range a val1 val2)
  (or
   (and (>= a val1)
        (<= a val2))
   (and (>= a val2)
        (<= a val1))))


(define (mean vals)
  (/ (fold + 0 vals)
     (length vals)))


(define (flatten-sublists l)

  (define (listify a)
    (if (list? a)
        a
        (list a)))

  (fold (lambda (a prev)
          (append prev (listify a)))
        '() l))


(define (more-than-one a)
  (if (nil? a)
      #f
      (not (nil? (cdr a)))))
