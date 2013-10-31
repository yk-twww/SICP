
(define cc
  (lambda (amount coin-values)
    (cond
      ((= amount 0) 1)
      ((or (< amount 0) (no-more? coin-values)) 0)
      (else
       (+ (cc amount
              (except-first-denomination coin-values))
          (cc (- amount
                 (first-denomination coin-values))
              coin-values))))))

(define no-more?
  (lambda (coin-values)
    (null? coin-values)))

(define first-denomination
  (lambda (coin-values)
    (car coin-values)))

(define except-first-denomination
  (lambda (coin-values)
    (cdr coin-values)))
