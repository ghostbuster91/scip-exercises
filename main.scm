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

