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

(display "square-list:")
(newline)
(square-list (list 1 2 3 4 5))

(define (square-list2 items)
  (cond ((null? items) nil)
        (else (cons (* (car items) (car items)) 
                    (square-list2 (cdr items))))
        )
  )

(square-list2 (list 1 2 3 4 5))
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

;; 2.29

(define (make-mobile left right)
    (list left right)
  )

(define (make-branch length structure)
    (list length structure)
  )

(define (left-branch structure)
  (car structure))

(define (right-branch structure)
  (car (cdr structure)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight-branch branch)
    (let ((struct (branch-structure branch)))
        (if (pair? struct)
          (total-weight struct)
          struct)
      )
  )

(define (total-weight structure)
  (let ((left (left-branch structure))
        (right (right-branch structure)))
       (+ (total-weight-branch left) (total-weight-branch right))
  )
)

(define s1 (make-branch 2 3))
(branch-structure s1)
(branch-length s1)
(total-weight-branch s1)

(define s3 (make-mobile (make-branch 1 4) (make-branch 2 (make-mobile (make-branch 3 3) (make-branch 2 5)))))
(total-weight s3)
;; c)

(define (total-force-branch branch)
    (let ((struct (branch-structure branch))
          (len (branch-length branch)))
        (if (pair? struct)
          (* len (total-force struct))
          (* len struct))
      )
  )

(define (total-force structure)
  (let ((left (left-branch structure))
        (right (right-branch structure)))
       (+ (total-force-branch left) (total-force-branch right))
  )
)

(total-force-branch s1)
(total-force s3)

(define (is-equal-branch? branch)
    (let ((struct (branch-structure branch)))
        (if (pair? struct)
          (is-equal? struct)
          true)
      )
  )

(define (is-equal? structure)
   (and (= (total-force-branch (left-branch structure)) (total-force-branch (right-branch structure))) (and (is-equal-branch? (left-branch structure)) (is-equal-branch? (right-branch structure)))) 
  )


(is-equal? s3)
(define s4 (make-mobile (make-branch 2 18) (make-branch 1 (make-mobile (make-branch 3 6) (make-branch 2 9)))))
(is-equal? s4)

;; d) representation change
;; A: not much, only right-branch and branch-structure selectors

;; 

(define (scale-list ls factor)
  (if (null? ls)
    nil
    (cons (* (car ls) factor)
          (scale-list (cdr ls) factor))
    )
  )

(scale-list (list 1 2 3 4 5) 10)

(define (scale-list2 ls factor)
  (map (lambda (x) (* x factor)) ls)
)

(scale-list2 (list 1 2 3 4 5) 10)


(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor) 
                    (scale-tree (cdr tree) factor)))
        )
)

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;; 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree)) 
                    (square-tree (cdr tree))))
        )
  )


(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; 2.31

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree)) 
                    (tree-map f (cdr tree))))
        )
  )

(define (square-tree2 tree)
  (tree-map (lambda (x) (* x x)) tree))


(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; 2.32

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list))
(subsets (list 1))
(subsets (list 1 2))

(subsets (list 1 2 3))

;; 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial 
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))


(define (scale-list3 ls factor)
  (map2 (lambda (x) (* x factor)) ls)
)


(scale-list3 (list 1 2 3 4 5) 10)

(define (append2 seq1 seq2)
  (accumulate (lambda (i acc) (cons i acc)) seq2 seq1))

(append (list 1 2 3) (list 1 4 (list 1 4 3)))
(append2 (list 1 2 3) (list 1 4 (list 1 4 3)))

(define (length3 seq)
  (accumulate (lambda (i acc) (+ 1 acc)) 0 seq))

(length3 (list 1 2 3 55 10))

;; 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (* (+ this-coeff higher-terms) x)) 
              0 
              coefficient-sequence)
  )

(horner-eval 2 (list 1 3 0 5 0 1))


;; 2.35 

(define (count-leaves2 tree)
    (accumulate (lambda (i acc) (+ (length i) acc)) 0 (map fringe tree))
  )

(count-leaves2 tree1)
(count-leaves2 (list tree1 tree1))

;; 2.36

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
      nil 
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs))
      )
  )
)

(define s (list (list 1 2 3) 
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))
(accumulate-n + 0 s)


;; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mx) (dot-product mx v)) m))

(define (transpose mat)
    (accumulate-n cons nil mat)
  )

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mx) (map (lambda (cx) (dot-product mx cx)) cols)) m)
    )
  )

(define m1 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define m2 (list (list 2) (list 1) (list 5)))

(transpose m2)
(transpose m1)
(transpose (list (list 3 1) (list 2 1) (list 1 0)))

(dot-product (list 1 3 -5) (list 4 -3 -1))
(matrix-*-vector (list (list 10 6 7) (list 3 9 1) (list 5 5 5)) (list 9 5 5)) 

(matrix-*-matrix (list (list 1 0 2) (list -1 3 1)) (list (list 3 1) (list 2 1) (list 1 0)))

;; 2.38

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))
    )
  )
  (iter initial seq)
  )


(define (fold-right op initial seq)
  (accumulate op initial seq))

(fold-right / 1 (list 1 2 3)) ;; 3/1 -> 2/3 -> 1/(2/3) = 1 * 3/2
(fold-left / 1 (list 1 2 3)) ;; 1/1 -> 1/2 -> (1/2)/3 = 1/6

(fold-right list nil (list 1 2 3)) ;; (3)-> (2 (3))-> (1 (2 (3)))
(fold-left list nil (list 1 2 3)) ;; (1) -> ((1) 2) -> (((1) 2) 3)

;; they have to be associative

;; 2.39

(define (reverse3 seq)
  (fold-right (lambda (x acc) (append acc (list x))) nil seq)
  )

(define (reverse4 seq)
  (fold-left (lambda (acc x) (cons x acc)) nil seq))


(reverse3 (list 1 2 3 4))
(reverse4 (list 1 2 3 4))

;;

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high)))
  )

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (generate-pairs n)
  (flatmap 
    (lambda (i) (map 
                  (lambda (j) (list i j)) 
                  (enumerate-interval 1 (- i 1))
                )
    )
    (enumerate-interval 1 n)
   )
  )

(generate-pairs 6)

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


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair )
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
              (generate-pairs n) 
       )
    )
  )

(prime-sum-pairs 6)


;; 2.40 
;; done see generate-pairs

;; 2.41

(define (generate-tripples n)
  (flatmap 
    (lambda (i) (flatmap
                  (lambda (j) 
                    (map 
                      (lambda (k) (list i j k))
                      (enumerate-interval 1 (- i 1))
                    )
                  ) 
                  (enumerate-interval 1 (- i 1))
                )
    )
    (enumerate-interval 1 n)
   )
  )

(generate-tripples 5)
(caddr (generate-tripples 5))

(define (make-triple-sum triple)
  (list (car triple) (cadr triple) (caddr triple) (+ (car triple) (cadr triple) (caddr triple))))

(define (tri-sum-less n s)
  (map make-triple-sum 
       (filter (lambda (x) (= (+ (car x) (cadr x) (caddr x)) s)) 
               (generate-tripples 6)
        )
   )
)

(tri-sum-less 6 10)

;; 2.42

(define (queens board-size)
    (define (queen-cols k)
      (if (= k 0)
        (list empty-board)
        (filter 
          (lambda (positions) (safe? k positions))
          (flatmap 
            (lambda (rest-of-queens) 
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols - k 1))))
      )
    (queen-cols board-size)
)

