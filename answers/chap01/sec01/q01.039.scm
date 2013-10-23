
(define cont-frac-iter-f
  (lambda (x)
    (lambda (n-f d-f k)
      (let _iter ((counter k)
                  (accum 0))
        (if (= counter 0)
            accum
            (_iter (- counter 1)
                   (/ ((n-f counter) x)
                      (+ ((d-f counter) x) accum))))))))

(define tan-cf
  (lambda (x k)
    ((cont-frac-iter-f x) nume-f deno-f k)))

(define nume-f
  (lambda (n)
    (lambda (x)
      (if (= n 1)
          x
          (- (* x x))))))

(define deno-f
  (lambda (n)
    (lambda (x)
      (- (* 2 n)
         1))))
