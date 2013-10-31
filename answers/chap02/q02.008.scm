
(define make-interval
  (lambda (a b)
    (cons a b)))

(define upper-bound
  (lambda (interval)
    (cdr interval)))

(define lower-bound
  (lambda (interval)
    (car interval)))


(define sub-interval
  (lambda (x y)
    (let ((p1 (- (upper-bound x) (lower-bound y)))
          (p2 (- (lower-bound x) (upper-bound y))))
      (make-interval p1 p2))))
