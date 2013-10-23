
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
              next
              (try next)))))
    (try first-guess)))

(define transe
  (lambda (x)
    (+ 1 (/ 1 x))))

;(fixed-point transe 1.0) => 1.6180555...
