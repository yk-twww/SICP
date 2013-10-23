
(define cont-frac
  (lambda (n d k)
    (let _iter ((counter 1))
      (if (= counter k)
          (/ (n k) (d k))
          (/ (n counter)
             (+ (d counter)
                (_iter (+ counter 1))))))))

(define cont-frac-iter
  (lambda (n d k)
    (let _iter ((counter k)
                (accum 0))
      (if (= counter 0)
          accum
          (_iter (- counter 1)
                 (/ (n counter)
                    (+ (d counter) accum)))))))

(define calc-golden-ratio
  (lambda (n)
    (let ((reversed (cont-frac-iter (lambda (n) 1.0) (lambda (n) 1.0) n)))
      (/ 1 reversed))))


;; golden-ration is 1.6180339
;;(calc-golden-ration 11) => 1.6179775280898876
;;(calc-golden-ration 12) => 1.6180555555555558
