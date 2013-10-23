
(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(define sqrt-iter
  (lambda (guess x)
    (let ((next (improve guess x)))
      (if (good-enough? guess next)
	  next
	  (sqrt-iter next x)))))

(define good-enough?
  (lambda (x y)
    (< (abs (- x y)) 0.001)))

(define improve
  (lambda (guess x)
    (avarage guess (/ x guess))))

(define avarage
  (lambda (x y)
    (/ (+ x y) 2)))

