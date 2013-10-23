
(define filtered-accumulate
  (lambda (combiner null-value term a next b filter)
    (if (> a b)
        null-value
        (if (filter a)
            (combiner (term a)
                      (filtered-accumulate combiner
                                           null-value
                                           term
                                           (next a)
                                           next
                                           b
                                           filter))
            (filtered-accumulate combiner
                                 null-value
                                 term
                                 (next a)
                                 next
                                 b
                                 filter)))))

(define filtered-accumulate-iter
  (lambda (combiner null-value term a next b filter)
    (let _iter ((current a)
                (result null-value))
      (if (> current b)
          result
          (if (filter current)
              (_iter (next current)
                     (combiner (term current) result))
              (_iter (next current)
                     result))))))
