
(define square
  (lambda (x)
    (* x x)))

(define square-sum
  (lambda (x y)
    (+ (square x) (square y))))

(define bigger-square-sum
  (lambda (a b c)
    (if (> a b)
	(if (> b c)
	    (square-sum a b)
	    (square-sum a c))
	(if (> a c)
	    (square-sum b a)
	    (square-sum b c)))))
