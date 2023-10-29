(define (mk-interval a b)
  (cons a b)
  )

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x)
)

(define (add-interval x y)
  (mk-interval (+ (lower-bound x) (lower-bound y))
               (+ (upper-bound x) (upper-bound y)))
  )

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (mk-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))
  )
  )

(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
    (error "Cannot divide by zero") 
    (mul-interval x (mk-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))
    )
)

;; 2.8

(define (sub-interval x y)
  (mk-interval (- (lower-bound x) (upper-bound y))
               (- (upper-bound x) (lower-bound y))
  )
  )

;; 2.9 

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2)
  )

(define (print-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]")
)

(define i1 (mk-interval 3.25 3.5))
(define i2 (mk-interval -2.0 -1.0))

(print-interval (add-interval i1 i2))

(define i3 1)

;; 2.11

(define (mk-center-width c w)
  (mk-interval (- c w) (+ c w))
  )

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0)
  )

(define (cwidth i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0)
  )


(center (add-interval i1 i2))
(cwidth (add-interval i1 i2))

;; 2.12

(define (mk-center-percent c p)
  (mk-interval (- c (* c p)) (+ c (* c p)))
)

(define (percent i)
  (/ (cwidth i) (center i)))

(define i3 (mk-center-percent 3 0.05))

(print-interval i3)
(center i3)
(percent i3)
