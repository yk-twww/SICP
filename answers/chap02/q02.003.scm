
(define make-point
  (lambda (x y)
    (cons x y)))

(define x-point
  (lambda (p)
    (car p)))

(define y-point
  (lambda (p)
    (cdr p)))


(define make-rect
  (lambda (p1 p2)
    (cons p1 p2)))

(define rect-width
  (lambda (rect)
    (abs (- (x-point (car rect))
            (x-point (cdr rect))))))

(define rect-height
  (lambda (rect)
    (abs (- (y-point (car rect))
            (y-point (cdr rect))))))

(define rect-perimeter
  (lambda (rect)
    (* 2
       (+ (rect-height rect)
          (rect-width  rect)))))

(define rect-area
  (lambda (rect)
    (* (rect-height rect)
       (rect-width  rect))))
                     