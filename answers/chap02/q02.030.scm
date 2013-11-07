
(define square-tree
  (lambda (tree)
    (cond
      ((null? tree) tree)
      ((not (pair? tree))
       (* tree tree))
      (else
       (cons (square-tree (car tree))
             (square-tree (cdr tree)))))))

(define square-tree2
  (lambda (tree)
    (map (lambda (subtree)
           (if (pair? subtree)
               (square-tree2 subtree)
               (* subtree subtree)))
         tree)))
