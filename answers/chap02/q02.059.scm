
(define element-of-set?
  (lambda (x set)
    (cond
      ((null? set) #f)
      ((equal? x (car set)) #t)
      (else (element-of-set? x (cdr set))))))

(define union-set
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((element-of-set? (car set1) set2)
           (union-set (cdr set1) set2))
          (else (union-set (cdr set1) (cons (car set1) set2))))))
