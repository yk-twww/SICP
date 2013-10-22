
(define fast-expt2
  (lambda (b n)
    (let _expt ((base b)
                (index n)
                (prod 1))
      (if (= index 0)
          prod
          (if (= (remainder index 2) 0)
              (_expt (* base base) (/ index 2) prod)
              (_expt base (- index 1) (* prod base)))))))
