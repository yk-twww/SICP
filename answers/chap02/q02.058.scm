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
      (else
       (append (if (pair? a1) a1 (list a1))
               '(+)
               (if (pair? a2) a2 (list a2)))))))
(define sum?
  (lambda (x)
    (and (pair? x) (member '+ x))))
(define addend
  (lambda (s)
    (car (separate-exp s '+))))
(define augend
  (lambda (s)
    (cadr (separate-exp s '+))))


(define product?
  (lambda (x)
    (and (pair? x) (member '* x))))
(define multiplier
  (lambda (p)
    (car (separate-exp p '*))))
(define multiplicand
  (lambda (p)
    (cadr (separate-exp p '*))))
(define make-product
  (lambda (m1 m2)
    (cond
      ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      ((or (sum? m1) (sum? m2))
       (cond
         ((and (sum? m1) (sum? m2))
          (list m1 '* m2))
         ((sum? m1)
          (cons m1 (append '(*)
                           (if (pair? m2) m2 (list m2)))))
         (else
          (append (if (pair? m1) m1 (list m1))
                  '(*)
                  (list m2)))))
      (else (append (if (pair? m1) m1 (list m1))
                    '(*)
                    (if (pair? m2) m2 (list m2)))))))


(define exponentiation?
  (lambda (x)
    (and (pair? x) (member '** x))))
(define base
  (lambda (e)
    (car (separate-exp e '**))))
(define exponent
  (lambda (e)
    (cadr (separate-exp e '**))))
(define make-exponentiation
  (lambda (base exponent)
    (cond
      ((=number? base 1) 1)
      ((=number? exponent 1) base)
      ((=number? exponent 0) 1)
      ((and (number? base) (number? exponent)) (expt base exponent))
      (else (list base '** exponent)))))


(define separate-exp
  (lambda (e op)
    (let _iter ((frst '())
                (sub-e e))
      (if (and (symbol? (car sub-e)) (eq? (car sub-e) op))
          (list (trim-parenth frst)
                (trim-parenth (cdr sub-e)))
          (_iter (append frst (list (car sub-e)))
                 (cdr sub-e))))))

(define trim-parenth
  (lambda (e)
    (if (pair? (car e))
        (if (null? (cdr e))
            (car e)
            e)
        (if (null? (cdr e))
            (car e)
            e))))

(define e1 '(x + 3 * (x + y + 2)))
(define e2 '(y ** 2 * x ** 3 + (y ** 2 + 3) * x + 3))
