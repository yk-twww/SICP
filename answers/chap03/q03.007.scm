
;; work, but not perfect
(define make-account
  (lambda (balance password)
    (define pass-ls (list password))
    (define auth
      (lambda (input-password)
        (memq input-password pass-ls)))
    (define add-password
      (lambda (new-pass)
        (set! pass-ls (cons new-pass pass-ls))))
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
              ((eq? m 'add-password) add-password)
              (else (lambda (x) "Unknown request")))
            (lambda (x) "Incorrect password"))))
    dispatch))

(define make-joint
  (lambda (acc current-pass new-pass)
    ((acc current-pass 'add-password) new-pass)
    acc))
    