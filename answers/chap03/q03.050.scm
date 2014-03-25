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
; utility function
(define list->stream
  (lambda (ls)
    (if (null? ls)
        '()
        (cons-stream (car ls)
                     (list->stream (cdr ls))))))


(define stream-map
  (lambda (proc . argstreams)
    (if (stream-null? (car argstreams))
        empty-stream
        (cons-stream
         (apply proc (map car-stream argstreams))
         (apply stream-map
                (cons proc (map cdr-stream argstreams)))))))