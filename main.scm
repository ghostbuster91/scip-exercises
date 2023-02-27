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

;; 1.16
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

;; 1.19
(define (fast-fib n)
  (fib-iter2 1 0 0 1 n))

(define (fib-iter2 a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter2 a b (+(* p p) (* q q)) (+(* 2 p q) (* q q)) (/ count 2) ))
        (else (fib-iter2 (+ (* b q) (* a q ) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1))
        )
  )
)

(fast-fib 70)

;;
;; normal execution order - first unfold then reduce
;; applicative order evaluation - first reduce then unfold (used in interpreter)

(define (gcd a b)
  (if (= b 0) a 
    (gcd b (reminder a b)))
)
;; 1.20 start
;; gcd 206 40

;; normal
;; gcd a=40 b=(reminder 206 40)
;; if b=(reminder 206 40)=6 
;; gcd a=(reminder 206 40) b=(reminder 40 (reminder 206 40))
;; if b=(reminder 40 (reminder 206 40))=4
;; gcd a=(reminder 40 (reminder 206 40)) b=(reminder (reminder 206 40) (reminder 40 (reminder 206 40)))
;; if b=(reminder (reminder 206 40) (reminder (reminder 206 40) (reminder 40 (reminder 206 40))))=2
;; gcd a=(reminder (reminder 206 40) (reminder 40 (reminder 206 40))) b=(reminder (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))))
;; if b=(reminder (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))))=0 (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))=2

;; applicative
;; gcd 40 (reminder 206 40)=6
;; if 6 
;; gcd 6 (reminder 40 6)=4
;; if 4
;; gcd 4 (reminder 6 4)=2
;; if 2
;; gcd a=2 b=(reminder 4 2)
;; if 0 => 2 

;; 1.20 end


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1))))
  )

(define (divides? a b )
  (= (remainder b a ) 0)
  )

(define (prime? a)
  (= (smallest-divisor a) a))

;; (x * y) mod m = [(x mod m) * (y mod m)] mod m
(define (expmod base expp m)
  (cond ((= expp 0) 1)
        ((even? expp)
         (remainder (square (expmod base (/ expp 2) m)) m)
         )
        (else (remainder (* base (expmod base (- expp 1) m)) m))
        )
  )

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)
        )
  )


(prime? 3)
(prime? 5)
(prime? 8)

(fast-prime? 3 7)
(fast-prime? 5 7)
(fast-prime? 8 7)
(fast-prime? 81 7)
(fast-prime? 971 7)


;; 1.21

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; 1.22

(define (timed-prime-test n)
  (start-prime-test n (runtime))
  )

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time)) false)
  )

(define (report-prime n elapsed-time)
  (newline)
  (display "**** ")
  (display n)
  (display " >> ")
  (display elapsed-time)
  true
  )

(timed-prime-test 77)
(timed-prime-test 97)

(define (search-for-primes n count)
 (if(= count 0) "end" (
                       if (timed-prime-test n) 
                        (search-for-primes (+ n 1) (- count 1))
                        (search-for-primes (+ n 1) count)
                      )
                )
 )


(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)

;; 1.22 end

;; 1.23 
(define (smallest-divisor2 n)
  (find-divisor2 n 2))

(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor2 n (next test-divisor))))
  )

(define (next n)
  (if (= n 2) 3 (+ n 2))
  )

(define (timed-prime-test2 n)
  (start-prime-test2 n (runtime))
  )

(define (start-prime-test2 n start-time)
  (if (prime2? n)
    (report-prime n (- (runtime) start-time)) false)
  )

(define (prime2? a)
  (= (smallest-divisor2 a) a))

(define (search-for-primes2 n count)
 (if(= count 0) "end" (
                       if (timed-prime-test2 n) 
                        (search-for-primes2 (+ n 1) (- count 1))
                        (search-for-primes2 (+ n 1) count)
                      )
                )
 )

(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)

;; 1.24

;; Yes, we could but then we would be operating on numbers >> m which is worse than the current impl. 

;; 1.27 


(define (fermat-test2 n m)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it m)
  )

(define (full-fermat n)
    (define (full-test n a)
      (cond ((= n a) true)
            ((fermat-test2 n a) (full-test n (+ a 1)))
            (else false)
      )
    )
    (full-test n 2)
  )

(full-fermat 201)
(full-fermat 17)
(full-fermat 561)
(full-fermat 1105)
(full-fermat 1729)

;; 1.28


(define (miller-test n)
  (define (try-it a)
    (define (check-it x) 
       (and (not (= x 0)) (= x 1)))
    (check-it (miller-expmod a (- n 1) n))
    )
  (try-it (+ 1 (random (- n 1))))
  )


(define (miller-expmod base expp m)
  (define (test-square x)
      (define (check-nontrivial-sqrt1 x square) 
       (if (and (= square 1) 
                (not (= x 1)) 
                (not (= x (- m 1)))) 
           0 
           square)
       )
    (check-nontrivial-sqrt1 x (remainder (square x) m))
    )

  (cond ((= expp 0) 1)
        ((even? expp)
         (test-square (miller-expmod base (/ expp 2) m))
         )
        (else (remainder (* base (miller-expmod base (- expp 1) m)) m))
        )
  )

(define (full-miller n count)
  (cond ((= count 0) true)
        ((miller-test n) (full-miller n (- count 1)))
        (else false))
  )

(full-miller 201 100)
(full-miller 17 100)
(full-miller 561 100)
(full-miller 1105 100)
(full-miller 1729 100)


;; 1.28 end



