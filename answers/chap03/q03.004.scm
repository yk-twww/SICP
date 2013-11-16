
(define make-account
  (lambda (balance password)
    (define auth
      (lambda (input-password)
        (eq? password input-password)))
    (define call-the-cops
      (lambda () "call the cops"))
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
    (let ((failure 0))
      (define dispatch
        (lambda (input-password m)
          (if (auth input-password)
              (cond
                ((eq? m 'withdraw) (begin (set! failure 0) withdraw))
                ((eq? m 'deposit) (begin (set! failure 0) deposit))
                (else (begin (set! failure 0) (lambda (x) "Unknown request"))))
              (lambda (x)
                (set! failure (+ failure 1))
                (if (>= failure 7)
                    (call-the-cops)
                    "Incorrect password")))))
      dispatch)))
