
(define (add-rat x y)
  (make-rat (+ (*(numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y)))
  )

(define (sub-rat x y)
  (make-rat (- (*(numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y)))
  )

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))
  )

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y)))
  )

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x)))
  )

(define (make-rat n d)
  (let ((g (gcd n d))
        (s (cond ((> (* n d) 0) 1)
                 (else -1))))
    (cons (* s (abs (/ n g))) (abs (/ d g))))
  )

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)

(define one-half (make-rat -2 -4))

(print-rat one-half)

;; 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
)

(define (mk-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p)
  )

(define (mk-segment p1 p2)
  (cons p1 p2)
  )

(define (start-segment s)
  (car s)
  )

(define (end-segment s)
  (cdr s)
  )

(define (midpoint-segment s)
  (define (midpoint a b)
    (/ (- a b) 2.0)
  )
  (mk-point (midpoint (x-point (end-segment s)) (x-point (start-segment s)))
            (midpoint (y-point (end-segment s)) (y-point (start-segment s))))
)

(define p1 (mk-point 1.0 1.0))
(define p2 (mk-point 5.0 5.0))

(define s (mk-segment p1 p2))

(print-point (midpoint-segment s))

(print-point (end-segment s))

;; 2.4 2.5 2.6 done on paper


