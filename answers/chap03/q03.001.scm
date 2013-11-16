
(define accumulator
  (lambda (init)
    (let ((sum init))
      (lambda (val)
        (set! sum (+ sum val))
        sum))))
