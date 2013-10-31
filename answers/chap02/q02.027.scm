
(define deep-reverse
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((pair? tree)
       (if (null? (cdr tree))
           tree
           (cons (deep-reverse (cadr tree))
                 (list (deep-reverse (car tree))))))
      (else tree))))

(define deep-reverse2
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((pair? tree)
       (append (deep-reverse2 (cdr tree))
               (list (deep-reverse2 (car tree)))))
      (else tree))))
