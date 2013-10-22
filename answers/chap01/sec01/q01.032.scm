
(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner
                             null-value
                             term
                             (next a)
                             next
                             b)))))

(define accumulate-iter
  (lambda (combiner null-value term a next b)
    (let _iter ((current a)
                (result null-value))
      (if (> current b)
          result
          (_iter (next current)
                 (combiner (term current) result))))))
