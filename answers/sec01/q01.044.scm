
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define repeated
  (lambda (f n)
    (let _iter ((counter n)
                (func f)
                (res-f (lambda (n) n)))
      (cond
        ((= counter 0) res-f)
        ((= (remainder counter 2) 0)
         (_iter (/ counter 2) (compose func func) res-f))
        (else
         (_iter (- counter 1) func (compose res-f func)))))))

(define smooth
  (lambda (f dx)
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
         3))))

(define n-smooth
  (lambda (f dx n)
    ((repeated (lambda (f) (smooth f dx)) n) f)))
