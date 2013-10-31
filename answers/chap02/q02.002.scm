
(define average
  (lambda (a b)
    (/ (+ a b) 2.0)))


(define make-point
  (lambda (x y)
    (cons x y)))

(define x-point
  (lambda (p)
    (car p)))

(define y-point
  (lambda (p)
    (cdr p)))

(define middle-point
  (lambda (p1 p2)
    (let ((mid-x (average (x-point p1)
                          (x-point p2)))
          (mid-y (average (y-point p1)
                          (y-point p2))))
      (make-point mid-x mid-y))))


(define make-segment
  (lambda (st-p end-p)
    (cons st-p end-p)))

(define start-segment
  (lambda (seg)
    (car seg)))

(define end-segment
  (lambda (seg)
    (cdr seg)))

(define midpoint-segment
  (lambda (seg)
    (middle-point (start-segment seg)
                  (end-segment seg))))
