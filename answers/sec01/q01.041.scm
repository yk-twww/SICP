
(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))
