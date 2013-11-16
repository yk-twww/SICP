
(define make-account
  (lambda (balance password)
    (define auth
      (lambda (input-password)
        (eq? password input-password)))
    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
            (begin
              (set! balance (- balance amount))
              balance)
            "Insufficient funds")))
    (define deposit
      (lambda (amount)
        (set! balance (+ balance amount))
        balance))
    (define dispatch
      (lambda (input-password m)
        (if (auth input-password)
            (cond
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (lambda (x) "Unknown request")))
            (lambda (x) "Incorrect password"))))
    dispatch))
        
            