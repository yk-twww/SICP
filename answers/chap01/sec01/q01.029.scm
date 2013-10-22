
(define simpson
  (lambda (f a b n)
    (let* ((diff (/ (- b a) n))
           (f-val (lambda (k) (f (+ a (* k diff))))))
      (* (/ diff 3)
         (sum (lambda (m)
                (cond
                  ((= m 0) (f-val 0))
                  ((= m n) (f-val n))
                  ((= (remainder m 2) 0)
                   (* 2 (f-val m)))
                  (else
                   (* 4 (f-val m)))))
              0
              (lambda (m) (+ m 1))
              n)))))

(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)))))
