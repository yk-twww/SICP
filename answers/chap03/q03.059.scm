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
(define scale-stream
  (lambda (s factor)
    (stream-map (lambda (x) (* x factor)) s)))
(define add-streams
  (lambda (s1 s2)
    (stream-map + s1 s2)))
(define mul-streams
  (lambda (s1 s2)
    (stream-map * s1 s2)))

(define one (cons-stream 1 one))
(define integer (cons-stream 1 (add-streams one integer)))


;a
(define inverse-stream
  (lambda (s)
    (cons-stream (/ 1 (car-stream s))
                 (inverse-stream (cdr-stream s)))))
(define integrate-series
  (lambda (series)
    (mul-streams series (inverse-stream integer))))

;b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 0 (integrate-series sine-series)))
(define sine-series
  (cons-stream 1 (scale-stream (integrate-series cosine-series) -1)))
;(stream->list cosine-series 20)


(define mul-series
  (lambda (s1 s2)
    (let ((car1 (car-stream s1))
          (car2 (car-stream s2))
          (cdr1 (cdr-stream s1))
          (cdr2 (cdr-stream s2)))
      (cons-stream (* car1 car2)                      
                   (add-streams (add-streams (scale-stream cdr1 car2)
                                             (scale-stream cdr2 car1))
                                (cons-stream 0 (mul-series cdr1 cdr2)))))))

(define invert-unit-series
  (lambda (s)
    (cons-stream 1
                 (scale-stream (mul-series (cons-stream 0 (cdr-stream s))
                                           (invert-unit-series s))
                               -1))))
;(stream->list (invert-unit-series exp-series) 20)
