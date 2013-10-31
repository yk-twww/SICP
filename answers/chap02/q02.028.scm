
(define fringe
  (lambda (ls)
    (cond
      ((null? ls) ls)
      ((pair? ls)
       (append (fringe (car ls))
               (fringe (cdr ls))))
      (else (list ls)))))
