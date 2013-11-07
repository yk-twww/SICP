
(define make-from-mag-ang
  (lambda (r a)
    (lambda (op)
      (cond
        ((eq? op 'real-part) (* r (cos a)))
        ((eq? op 'imag-part) (* r (sin a)))
        ((eq? op 'magnitude) r)
        ((eq? op 'angle) a)
        (else
         (display "Unknown op -- MAKE-FROM-MAG-ANG"))))))

(define apply-generic
  (lambda (op arg)
    (arg op)))
