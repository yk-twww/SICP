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

(define stream-enumerate-interval
  (lambda (from to)
    (if (> from to)
        empty-stream
        (cons-stream from
                     (stream-enumerate-interval (+ from 1) to)))))
(define show
  (lambda (x)
    (display x)
    (newline)
    x))


(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)