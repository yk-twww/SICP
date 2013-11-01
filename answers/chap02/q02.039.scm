
(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define fold-right accumulate)

(define fold-left
  (lambda (op initial sequence)
    (let _iter ((result initial)
                (rest sequence))
      (if (null? rest)
          result
          (_iter (op result (car rest))
                 (cdr rest))))))

(define reverse-r
  (lambda (ls)
    (fold-right (lambda (x y) (append y (list x))) '() ls)))

(define reverse-l
  (lambda (ls)
    (fold-left (lambda (x y) (cons y x)) '() ls)))
