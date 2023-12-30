
(define (length2 items)
  (if (null? items)
    0
    (+ 1 (length2 (cdr items)))
    )
  )

;; 2.17

(define (last-pair list1)
  (define (go list2 last)
    (if (null? list2)
        last
        (go (cdr list2) (car list2))
      )
    )
  (go (cdr list1) (car list1))
)

(last-pair (list 1 12 35 55 66))

;; 2.18

(define (reverse2 list1)
  (if (> (length2 list1) 1)
    (cons (reverse2 (cdr list1)) (car list1))
    (car list1)
    )
  )

(reverse2 (list 1 12 35 55 66))

;; 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 5 2 1 0.5))

(define (no-more? list1)
  (null? list1))

(define (except-first-denomination list1)
  (cdr list1))

(define (first-denomination list1)
  (car list1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else 
        (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)
           )
          )
        )
  )


(cc 100 us-coins)
;; (cc 100 uk-coins)

;; 2.22

(define nil (list))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer (square (car things))))
      )
    )
  (iter items nil)
  )



(square-list (list 1 2 3 4 5))

;; 2.23

(define  (for-each2 f items)
  (define (next)
    (f (car items))
    (for-each2 f (cdr items))
  )
  (if (null? items)
    true
    (next)
  )
  true
  )

(for-each2 (lambda (x) (newline)(display x))
    (list 57 222 88))
