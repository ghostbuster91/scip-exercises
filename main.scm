(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))
  )
)

(fib 6)


(define (ff n)
  (if (< n 3) n (
                 + (ff(- n 1))
                   (* 2 (ff(- n 2)))
                   (* 3 (ff(- n 3)))
                )
  )
)

(ff 6)

(define (ff2 n)
  (ff2-iter 2 1 0 n)
)

(define (ff2-iter a b c n)
  (if (= n 0) 
    c
    (ff2-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))
  )
)

(ff2 6)

(define (expt b n)
    (if (= n 0)
      1
      (* b (expt b (- n 1)))
    )
)

(expt 4 2)

(define (expt2 b n)
  (expt-iter b n 1)
)

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b (- counter 1) (* b product))
  )
)

(expt2 4 10)

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))
  )
)

(fast-exp 2 4)
(fast-exp 2 5)
(fast-exp 2 6)
(fast-exp 2 7)

(define (fast-exp2 b n product)
  (cond ((= n 0) product)
        ((even? n) (fast-exp2 (square b) (/ n 2) product))
        (else (fast-exp2 b (- n 1) (* b product)))
  )
)

(fast-exp2 2 4 1)
(fast-exp2 2 5 1)
(fast-exp2 2 6 1)
(fast-exp2 2 7 1)
