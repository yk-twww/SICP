
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define repeated
  (lambda (f n)
    (cond
      ((= n 0) (lambda (x) x))
      ((= (remainder n 2) 0)
       (repeated (compose f f) (/ n 2)))
      (else
       (compose f (repeated f (- n 1)))))))

(define repeated-iter
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
