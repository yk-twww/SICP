
(define make-mentioned
  (lambda (f)
    (let ((mentioned 0))
      (lambda (v)
        (cond
          ((eq? v 'how-many-calls?) mentioned)
          ((eq? v 'reset-count) (set! mentioned 0))
          (else
           (set! mentioned (+ mentioned 1))
           (f v)))))))
