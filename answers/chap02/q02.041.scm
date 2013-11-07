
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

(define filter
  (lambda (predicate ls)
    (if (null? ls)
        '()
        (if (predicate (car ls))
            (cons (car ls)
                  (filter predicate (cdr ls)))
            (filter predicate (cdr ls))))))

(define sum
  (lambda (ls)
    (accumulate + 0 ls)))

(define unique-pairs
  (lambda (n)
    (accumulate (lambda (i result-pairs)
                  (append (map (lambda (j)
                                 (list i j))
                               (enumerate-interval (- i 1)))
                          result-pairs))
                '()
                (enumerate-interval n))))

(define unique-triples
  (lambda (n)
    (accumulate (lambda (i result-pairs)
                  (append (map (lambda (pair)
                                 (cons i pair))
                               (unique-pairs (- i 1)))
                          result-pairs))
                '()
                (enumerate-interval n))))

(define sum-triples
  (lambda (n s)
    (filter (lambda (ls)
              (= (sum ls) s))
            (unique-triples n))))
