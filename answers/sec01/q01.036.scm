
(define tolerance 0.0001)

(define fixed-point
  (lambda (f first-guess)
    (define close-enough?
      (lambda (v1 v2)
        (< (abs (- v1 v2)) tolerance)))
    (define try
      (lambda (guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              (begin
                (display next)
                (newline)
                next)
              (begin
                (display guess)
                (newline)
                (try next))))))
    (try first-guess)))

(define transe
  (lambda (x)
    (/ (log 1000) (log x))))

(define mean-transe
  (lambda (x)
    (/ (+ x (transe x)) 2)))

;;(fixed-point transe 3)      => 4.555563347820309   printed 28 times
;;(fixed-point mean-transe 3) => 4.5555490009596555  printed  6 times
