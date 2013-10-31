
(define for-each1
  (lambda (f ls)
    (if (null? ls)
        '()
        (begin
          (f (car ls))
          (for-each f (cdr ls))))))
