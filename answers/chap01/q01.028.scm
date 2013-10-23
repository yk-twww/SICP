
(use srfi-27)

(define miller-rabin
  (lambda (n)
    (cond
      ((= n 2) #t)
      ((= (remainder n 2) 0) #f)
      (else
       (let ((decomped (decomp n)))
         (let _test ((rand (gen-rand n))
                     (counter 0))
           (if (= counter 30)
               #t
             (if (miller-test rand decomped n)
                 (_test (gen-rand n) (+ counter 1))
                 #f))))))))

(define miller-test
  (lambda (base decomped n)
    (let ((init-base (expmod base (car decomped) n 1)))
      (if (= init-base 1)
          #t
          (let _iter ((index (cadr decomped))
                      (next-base init-base))
            (cond
              ((= index 0) #f)
              ((= next-base -1) #t)
              (else (_iter (- index 1)
                           (remainder (* next-base next-base) n)))))))))

(define find-sqrt
  (lambda (base index mod)
    (let ((decomped (decomp index)))
      (let ((new-base (expmod base (car decomped) mod 1))
            (new-index (cadr decomped)))
        (let _find ((base new-base)
                    (index new-index))
          (if (= index 0)
              #t
              (let ((next-base (remainder (* base base)
                                           mod)))
                (if (= next-base 1)
                    #f
                    (_find next-base (- index 1))))))))))

(define decomp
  (lambda (n)
    (let _decomp ((num n)
                  (index 0))
      (if (= (remainder num 2) 0)
          (_decomp (/ num 2) (+ index 1))
          (list num index)))))

(define expmod
  (lambda (base index mod prod)
    (cond
      ((= index 0) prod)
      ((= (remainder index 2) 0)
       (expmod (remainder (* base base) mod) (/ index 2) mod prod))
      (else (expmod base (- index 1) mod (remainder (* prod base)
                                                    mod))))))

(define gen-rand
  (lambda (n)
    (+ (random-integer (- n 1)) 1)))
(define gen-rand2
  (lambda (n)
    4))
