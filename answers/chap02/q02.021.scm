
(define square-list
  (lambda (items)
    (if (null? items)
        '()
        (cons (* (car items)
                 (car items))
              (square-list (cdr items))))))

(define square-list2
  (lambda (items)
    (map (lambda (x) (* x x)) items)))
