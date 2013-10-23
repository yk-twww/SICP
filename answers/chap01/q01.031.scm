
(define product
  (lambda (term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term (next a) next b)))))

(define product-iter
  (lambda (term a next b)
    (let _iter ((current a)
                (result 1))
      (if (> current b)
          result
          (_iter (next current) (* result (term current)))))))


(define appli-pi
  (lambda (n)
    (* (product-iter (lambda (x) (/ (* x (+ x 2))
                                    (* (+ x 1) (+ x 1))))
                     2
                     (lambda (m) (+ m 2))
                     n)
       4.0)))
