
(define prod
  (lambda (a b)
    (let _prod ((psv a)
                (atv b)
                (res 0))
      (if (= atv 0)
          res
          (if (= (remainder atv 2) 0)
              (_prod (double psv) (halve atv) res)
              (_prod psv (- atv 1) (+ res psv)))))))

(define double
  (lambda (a)
    (+ a a)))

(define halve
  (lambda (a)
    (/ a 2)))

  