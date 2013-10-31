

(define same-sign?
  (lambda (m n)
    (>= (* m n) 0)))

(define make-rat
  (lambda (n d)
    (let ((g (gcd n d)))
      (let ((abs-n (/ (abs n) g))
            (abs-d (/ (abs d) g)))
        (if (same-sign? n d)
            (cons abs-n abs-d)
            (cons (- abs-n) abs-d))))))
