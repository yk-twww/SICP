(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define accumulate-n
  (lambda (op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map (lambda (ls) (car ls))
                                       seqs))
              (accumulate-n op init (map (lambda (ls)(cdr ls))
                                         seqs))))))
(define dot-product
  (lambda (v w)
    (accumulate +
                0
                (map * v w))))

(define dot-product1
  (lambda (v w)
    (accumulate +
                0
                (accumulate-n *
                              1
                              (list v w)))))

(define matrix-*-vector
  (lambda (m v)
    (map (lambda (w) (dot-product w v)) m)))

(define transepose
  (lambda (m)
    (accumulate-n cons '() m)))

(define matrix-*-matrix
  (lambda (m n)
    (let ((cols (transepose n)))
      (map (lambda (row) (matrix-*-vector cols row)) m))))
