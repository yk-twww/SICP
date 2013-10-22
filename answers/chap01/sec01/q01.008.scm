
(define cubic
  (lambda (x)
    (cubic-iter 1.0 x)))

(define cubic-iter
  (lambda (guess x)
    (let ((next (improve guess x)))
      (if (good-enough? guess next)
	  next
	  (cubic-iter next x)))))

(define good-enough?
  (lambda (x y)
    (< (abs (- x y)) 0.001)))

(define improve
  (lambda (guess x)
    (/ (+ (/ x (* guess guess))
	  (* 2 guess))
       3)))
