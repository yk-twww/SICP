
(define deriv
  (lambda (expe var)
    (cond
      ((number? expe) 0)
      ((variable? expe)
       (if (same-variable? expe var) 1 0))
      ((sum? expe)
       (make-sum (deriv (addend expe) var)
                 (deriv (augend expe) var)))
      ((product? expe)
       (make-sum
        (make-product (multiplier expe)
                      (deriv (multiplicand expe) var))
        (make-product (deriv (multiplier expe) var)
                      (multiplicand expe))))
      ((exponentiation? expe)
       (make-product (exponent expe)
                     (make-product (make-exponentiation (base expe)
                                                        (make-sum (exponent expe) (- 1)))
                                   (deriv (base expe) var)))))))

(define variable?
  (lambda (x)
    (symbol? x)))

(define same-variable?
  (lambda (v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))))

(define =number?
  (lambda (x num)
    (and (number? x) (= x num))))


(define make-sum
  (lambda (a1 a2)
    (cond
      ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2)) (+ a1 a2))
      (else (list '+ a1 a2)))))
(define sum?
  (lambda (x)
    (and (pair? x) (eq? (car x) '+))))
(define addend
  (lambda (s)
    (cadr s)))
(define augend
  (lambda (s)
    (caddr s)))

(define product?
  (lambda (x)
    (and (pair? x) (eq? (car x) '*))))
(define multiplier
  (lambda (p)
    (cadr p)))
(define multiplicand
  (lambda (p)
    (caddr p)))
(define make-product
  (lambda (m1 m2)
    (cond
      ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      (else (list '* m1 m2)))))

(define exponentiation?
  (lambda (x)
    (and (pair? x) (eq? (car x) '**))))
(define base
  (lambda (e)
    (cadr e)))
(define exponent
  (lambda (e)
    (caddr e)))
(define make-exponentiation
  (lambda (base exponent)
    (cond
      ((=number? base 1) 1)
      ((=number? exponent 1) base)
      ((=number? exponent 0) 1)
      ((and (number? base) (number? exponent)) (expt base exponent))
      (else (list '** base exponent)))))
