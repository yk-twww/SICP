
(define func
  (lambda ()
    (let ((state 1))
      (lambda (x)
        (set! state (* x state))
        state))))

(define f (func))
