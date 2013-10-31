
(define last-pair
  (lambda (ls)
    (if (null? (cdr ls))
               ls
               (last-pair (cdr ls)))))
