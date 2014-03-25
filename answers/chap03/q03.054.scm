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
(define stream->list
  (lambda (s n)
    (cond ((stream-null? s) '())
          ((= n 0) '())
          (else
           (cons (car-stream s)
                 (stream->list (cdr-stream s) (- n 1)))))))

(define stream-map
  (lambda (proc . argstreams)
    (if (stream-null? (car argstreams))
        empty-stream
        (cons-stream
         (apply proc (map car-stream argstreams))
         (apply stream-map
                (cons proc (map cdr-stream argstreams)))))))
(define add-streams
  (lambda (s1 s2)
    (stream-map + s1 s2)))
(define mul-streams
  (lambda (s1 s2)
    (stream-map * s1 s2)))

(define one (cons-stream 1 one))
(define integer (cons-stream 1 (add-streams one integer)))

(define factorials (cons-stream 1 (mul-streams factorials integer)))


;(stream->list factorials 30)