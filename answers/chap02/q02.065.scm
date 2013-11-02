
(define entry (lambda (tree) (car tree)))
(define left-branch (lambda (tree) (cadr tree)))
(define right-branch (lambda (tree) (caddr tree)))
(define make-tree
  (lambda (entry left right)
    (list entry left right)))

(define tree->list
  (lambda (tree)
    (if (null? tree)
        '()
        (append (tree->list (left-branch tree))
                (cons (entry tree)
                      (tree->list (right-branch tree)))))))
(define list->tree
  (lambda (ls)
    (car (partial-tree ls (length ls)))))
(define partial-tree
  (lambda (elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts) right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts)))))))))


(define _union-set
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((null? set2) set1)
      (else
       (let ((x1 (car set1))
             (x2 (car set2)))
         (cond
           ((= x1 x2) (cons x1
                            (_union-set (cdr set1) (cdr set2))))
           ((< x1 x2) (cons x1
                            (_union-set (cdr set1) set2)))
           ((> x1 x2) (cons x2
                            (_union-set set1 (cdr set2))))))))))
(define _intersection-set
  (lambda (set1 set2)
    (cond
      ((or (null? set1) (null? set2)) '())
      (else
       (let ((x1 (car set1))
             (x2 (car set2)))
         (cond
           ((= x1 x2) (cons x1
                            (_intersection-set (cdr set1) (cdr set2))))
           ((> x1 x2) (_intersection-set set1 (cdr set2)))
           (else (_intersection-set (cdr set1) set2))))))))


(define union-set
  (lambda (set1 set2)
    (let ((s-ls1 (tree->list set1))
          (s-ls2 (tree->list set2)))
      (let ((union-ls (_union-set s-ls1 s-ls2)))
        (list->tree union-ls)))))
(define intersection-set
  (lambda (set1 set2)
    (let ((s-ls1 (tree->list set1))
          (s-ls2 (tree->list set2)))
      (let ((intersection-ls (_intersection-set s-ls1 s-ls2)))
        (list->tree intersection-ls)))))
