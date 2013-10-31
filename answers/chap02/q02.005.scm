
(define cons1
  (lambda (x y)
    (* (expt 2 x)
       (expt 3 y))))

(define car1
  (lambda (z)
    ((calc-index 2) z)))

(define cdr1
  (lambda (z)
    ((calc-index 3) z)))

(define calc-index
  (lambda (prime)
    (lambda (n)
      (let _div ((count 0)
                 (num n))
        (if (= (remainder num prime) 0)
            (_div (+ count 1) (/ num prime))
            count)))))
