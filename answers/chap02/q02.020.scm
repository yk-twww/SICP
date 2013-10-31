
(define same-parity
  (lambda (x . z)
    (let _iter ((ls (cons x z)))
      (cond
        ((null? ls) '())
        ((= (remainder (abs (- x (car ls)))
                       2)
            0)
         (cons (car ls) (_iter (cdr ls))))
        (else
         (_iter (cdr ls)))))))
