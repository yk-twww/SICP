
(define cons0
  (lambda (x y)
    (lambda (m)
      (m x y))))

(define car0
  (lambda (z)
    (z (lambda (p q) p))))

(define cdr0
  (lambda (z)
    (z (lambda (p q) q))))
