(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))
(define car-stream
  (lambda (stm)
    (car stm)))
(define cdr-stream
  (lambda (stm)
    (force (cdr stm))))
(define empty-stream '())
(define stream-null?
  (lambda (s)
    (equal? s empty-stream)))
(define stream-ref
  (lambda (s n)
    (if (= n 0)
        (car-stream s)
        (stream-ref (cdr-stream s) (- n 1)))))

(define stream-map
  (lambda (proc . argstreams)
    (if (stream-null? (car argstreams))
        empty-stream
        (cons-stream
         (apply proc (map car-stream argstreams))
         (apply stream-map
                (cons proc (map cdr-stream argstreams)))))))
(define scale-stream
  (lambda (s factor)
    (stream-map (lambda (x) (* x factor)) s)))


(define merge
  (lambda (s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (car-stream s1))
                 (s2car (car-stream s2)))
             (cond ((< s1car s2car)
                    (cons-stream s1car (merge (cdr-stream s1) s2)))
                   ((> s1car s2car)
                    (cons-stream s2car (merge (cdr-stream s2) s1)))
                   (else
                    (cons-stream s1car (merge (cdr-stream s1)
                                              (cdr-stream s2))))))))))

(define S (cons-stream 1 (merge (merge (scale-stream S 2)
                                       (scale-stream S 3))
                                (scale-stream S 5))))

