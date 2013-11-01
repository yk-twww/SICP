
(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define map1
  (lambda (p sequence)
    (accumulate (lambda (ele ls) (cons (p ele) ls)) '() sequence)))

(define append1
  (lambda (seq1 seq2)
    (accumulate cons seq2 seq1)))

(define length1
  (lambda (sequence)
    (accumulate (lambda (ele res) (+ 1 res)) 0 sequence)))
