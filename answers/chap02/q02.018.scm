
(define reverse1
  (lambda (ls)
    (if (null? ls)
        ls
        (append (reverse (cdr ls))
              (list (car ls))))))

(define reverse2
  (lambda (ls)
    (let _rev ((_ls ls)
               (store '()))
      (if (null? _ls)
          store
          (_rev (cdr _ls)
                (cons (car _ls)
                      store))))))
