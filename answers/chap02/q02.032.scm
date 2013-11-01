
(define subsets
  (lambda (s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
          (append rest (map (lambda (ls)
                              (cons  (car s) ls))
                  rest))))))
