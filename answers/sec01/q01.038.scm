
(define cont-frac-iter
  (lambda (n d k)
    (let _iter ((counter k)
                (accum 0))
      (if (= counter 0)
          accum
          (_iter (- counter 1)
                 (/ (n counter)
                    (+ (d counter) accum)))))))

(define nume
  (lambda (n)
    1))

(define deno
  (lambda (n)
    (if (= (remainder n 3) 2)
        (* 2 (+ (quotient n 3) 1))
        1)))


(define calc-e
  (lambda (n)
    (+ (cont-frac-iter nume deno n)
       2.0)))
