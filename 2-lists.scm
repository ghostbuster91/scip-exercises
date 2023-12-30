(define nil (list))

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
  (define (iter x answer)
    (if (null? x)
      answer
      (iter (cdr x) (cons (car x) answer)))
    )
 (iter list1 nil)
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

(define (count-leaves x)
    (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))
        )
  )

(define tree1 (cons (list 1 2) (list 3 4)))

(count-leaves tree1)
(count-leaves (list tree1 tree1))
(length2 tree1)

;; 2.24
(list 1 2 (list 3 4))

;; (list 1 (list 2 (list 3 4)))
;;  [1, [2 [3, 4]]]
;;   .
;; 1  .
;;   2 .
;;    3 4
;;

(count-leaves (list 1 2 (list 3 4)))
(length2 (list 1 2 (list 3 4)))

;; 2.25 
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

;; 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

;; append 
;; (1 2 3 4 5 6)
;; cons 
;; ((1 2 3) 4 5 6)
;; list 
;; ((1 2 3) (4 5 6))

(append x y)
(cons x y)
(list x y)

;; 2.27

(define (deep-reverse items)
  (define (helper x)
    (if (pair? x)
      (deep-reverse x)
      x
    )
  )
  (define (iter x answer)
    (if (null? x)
      answer
      (iter (cdr x) (cons (helper (car x)) answer)))
    )
 (iter items nil)
) 


(list (list 1 2) (list 3 4))
(deep-reverse (list (list 1 2) (list 3 4)))
(reverse2 (list (list 1 2) (list 3 4)))
(reverse2 (list (list 1 2)))


(deep-reverse (list (list 1 2) 3 4 (list 6 7)))

;; 2.28

(define (fringe items)
    (define (go x acc)
        (cond ((null? x) acc)
                ((not (pair? x)) (cons x acc))
                (else (append (go (car x) acc) (go (cdr x) acc)))
                )
      )
    (go items nil)
  )

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
