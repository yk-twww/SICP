
(define make-interval
  (lambda (a b)
    (cons a b)))

(define upper-bound
  (lambda (interval)
    (cdr interval)))

(define lower-bound
  (lambda (interval)
    (car interval)))
