
(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define enumerate-tree
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((not (pair? tree)) (list tree))
      (else (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree)))))))

(define count-leaves
  (lambda (t)
    (accumulate +
                0
                (map (lambda (x) 1)
                     (enumerate-tree t)))))
