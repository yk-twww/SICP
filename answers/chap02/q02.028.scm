
(define fringe
  (lambda (ls)
    (cond
      ((null? ls) ls)
      ((pair? ls)
       (append (fringe (car ls))
               (fringe (cdr ls))))
      (else (list ls)))))

(define fringe2
  (lambda (ls)
    (if (null? ls)
        ls
        (if (pair? (car ls))
            (fringe (append (car ls)
                            (cdr ls)))
            (cons (car ls)
                  (fringe (cdr ls)))))))
