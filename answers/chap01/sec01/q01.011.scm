
(define f-rec
  (lambda (n)
    (if (< n 3)
        n
        (+ (f-rec (- n 1))
           (* (f-rec (- n 2)) 2)
           (* (f-rec (- n 3)) 3)))))

(define f-iter
  (lambda (n)
    (let _iter ((a 0)
                (b 1)
                (c 2)
                (count 0))
      (if (= count n)
          a
          (_iter b
                 c
                 (+ c (* b 2) (* a 3))
                 (+ count 1))))))
