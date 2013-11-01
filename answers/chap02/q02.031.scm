
(define square-map
  (lambda (f tree)
    (cond
      ((null? tree) tree)
      ((not (pair? tree)) (f tree))
      (else
       (cons (square-map f (car tree))
             (square-map f (cdr tree)))))))
