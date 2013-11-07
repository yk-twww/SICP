
(define make-mobile
  (lambda (left right)
    (list left right)))

(define make-branch
  (lambda (length structure)
    (list length structure)))

(define left-branch
  (lambda (mobile)
    (car mobile)))

(define right-branch
  (lambda (mobile)
    (cadr mobile)))

(define branch-length
  (lambda (branch)
    (car branch)))

(define branch-structure
  (lambda (branch)
    (cadr branch)))
