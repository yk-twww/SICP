
(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define horner-eval
  (lambda (x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
                0
                coefficient-sequence)))
