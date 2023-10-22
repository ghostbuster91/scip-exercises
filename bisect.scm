(define (average neg-point pos-point)
  (/ (+ neg-point pos-point) 2)
)

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
       (if (close-enough? neg-point pos-point)
         midpoint
         (let ((test-value (f midpoint)))
              (cond ((positive? test-value)
                     (search f neg-point midpoint))
                    ((negative? test-value)
                     (search f midpoint pos-point))
                    (else midpoint)))
       )
   )
)

(define (f-example x)
  (- (* x x) 5))

(search f-example 0.0 10.0)

(define (half-interval-method f a b)
  (let ((fa (f a))
        (fb (f b)))
      (cond ((and (positive? fa) (negative? fb))
             (search f b a))
            ((and (negative? fa) (positive? fb))
             (search f a b))
            (else (error "wartosci funkcji nie maja przeciwnych znakow"))
            )
  )
)

(half-interval-method f-example 10.0 0.0)
;; (half-interval-method f-example 10.0 15.0)

(half-interval-method sin 2.0 4.0)

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
        next
        (try next))
    )
  )
  (try first-guess)
)

(fixed-point cos 1.0)

(define (msqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0)
  )

(msqrt 100.0)

;; 1.35 golden ratio
(fixed-point (lambda (y) (+ 1 (/ 1.0 y))) 1.0)

;; 1.36 skipped as it was simple

;; 1.37 

(define (cont-frac n d k)
  (define (go i)
    (/ (n i) (+ (d i) 
                (if (< i k) (go (+ i 1)) (/ (n k) (d k)))
             )
    )
  )
  (go 0)
)

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           1000)

(define (cont-frac-iter n d k)
  (define (iter acc i)
    (if (> i 0)
      (iter (/ (n i) (+ (d i) acc)) (- i 1))
      acc
    )
  )
  (iter (/ (n k) (d k)) k)
)

(cont-frac-iter (lambda (x) 1.0)
           (lambda (x) 1.0)
           1000)

;; 1.38
 (define (di i) ;  
   (if (= (remainder i 3) 2) 
       (/ (+ i 1) 1.5) 
       1)
   )

(cont-frac-iter (lambda (x) 1.0)
                di
                100)

;; 1.39

 (define (tan-cf x k) 
   (cont-frac (lambda (i) 
                (if (= i 1) x (- (* x x)))) 
              (lambda (i) 
                (- (* i 2) 1)) 
              k)) 

(tan-cf 1.0 1000)
;;
(define (average-damp f)
  (lambda (x) (average (f x) x))
)

(define (msqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0)
  )

(msqrt2 100.0)

;; Newton

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
  )

(define (cube x) (* x x x))

(cube 5)

((deriv cube) 5)

(define (newton-transform g) 
  (lambda (x) (- x (/ (g x) ((deriv g) x))))
  )

(define (newton-method f k)
  (fixed-point (newton-transform f) k)
  )

(define (msqrt3 x)
  (newton-method (lambda (y) (- (/ x y) y)) 1.0)
  )

(msqrt3 100.0)

;; 1.40

(define (cubic a b c) 
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
  )

(newton-method (cubic 1 2 3) 1.0)

;; 1.41

(define (twice f)
  (lambda (x) (f (f x)))
  )

(((twice (twice twice)) (lambda (x) (+ x 1))) 5)

;; 1.42

(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (inc x) (+ x 1))

((compose square inc) 6)

;; 1.43

(define (repeated f times)
  (if (= times 1) f (compose f (repeated f (- times 1))))
)

((repeated square 2) 5)

;; 1.44 smoothing

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0))
)

(define (multi-smooth f n)
  ((repeated smooth n) f)
  )

(define (smooth-example x)
  (if (< x 0.0) -1.0 1.0)
  )

(smooth-example 0.0)
((multi-smooth smooth-example 3) 0.0)
((multi-smooth smooth-example 5) 0.0)
((multi-smooth smooth-example 10) 0.0)

;; 1.45

(define (pow n k)
  (define (go acc t)
    (if (= t 0) acc (go (* acc n) (- t 1)))
  ) 
  (go 1 k)
)

(define (nsqrt x n) 
 (fixed-point ((repeated average-damp 1) (lambda (y) (/ x (pow y (- n 1))))) 1.0)
)

(pow 5 3)
(pow 5 2)
(pow 5 1)
(pow 5 0)

(nsqrt 100.0 2)
(nsqrt 1000.0 3)
(nsqrt 64.0 6)
(nsqrt 128.0 7)
(nsqrt (pow 5 3) 3)

;; 1.46

(define (iterative-improve good-enough? improve)  
  (define (iter guess)  
    (if (good-enough? guess)  
        guess  
        (iter (improve guess)))
  )
  iter
) 

(define (fixed-point2 f first-guess)
  ((iterative-improve (lambda (x) (close-enough? x (f x))) f) first-guess)
)

(define (msqrt4 x)
  (fixed-point2 (lambda (y) (average y (/ x y)))
               1.0)
  )

(msqrt4 100)


