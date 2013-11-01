
(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define accumulate-n
  (lambda (op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map (lambda (ls) (car ls))
                                       seqs))
              (accumulate-n op init (map (lambda (ls)(cdr ls))
                                         seqs))))))
