
(define prime?
  (lambda (n)
    (if (< n 2)
        #f
        (let ((upper (sqrt n)))
          (let _iter ((div-n 2))
            (if (> div-n upper)
                #t
                (if (= (remainder n div-n) 0)
                    #f
                    (_iter (+ div-n 1)))))))))

(define filter
  (lambda (predicate ls)
    (if (null? ls)
        '()
        (if (predicate (car ls))
            (cons (car ls)
                  (filter predicate (cdr ls)))
            (filter predicate (cdr ls))))))

(define enumerate-interval
  (lambda (n)
    (let _iter ((count n)
                (store '()))
      (if (= count 0)
          store
          (_iter (- count 1)
                 (cons count store))))))

(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define flatmap
  (lambda (proc seq)
    (accumulate append '() (map proc seq))))

(define prime-sum?
  (lambda (pair)
    (prime? (+ (car pair) (cadr pair)))))

(define make-pair-sum
  (lambda (pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))))



(define unique-pairs
  (lambda (n)
    (accumulate (lambda (i result-pairs)
                  (append (map (lambda (j)
                                 (list i j))
                               (enumerate-interval (- i 1)))
                          result-pairs))
                '()
                (enumerate-interval n))))

(define prime-sum-pairs
  (lambda (n)
    (map make-pair-sum
         (filter prime-sum?
                 (unique-pairs n)))))
