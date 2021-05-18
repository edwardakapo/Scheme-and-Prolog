#|
Author: Oluwademilade Edward Akapo
Student No: 101095403
|#

;part A
;1
(display "....question 1 tests......")(newline)

(define (list-till-elem x lis)
  (cond ((null? lis)'())
        ((eq? (car lis) x) '())
        (else (cons (car lis) (list-till-elem x (cdr lis))))))

(define (split-helper x lis lis-x)
  (cond ((null? lis-x) '())
        ((null? lis) (list lis-x))
        ((eq? (car lis) x) (if (not(eq? (list-till-elem x lis-x) '()))
                               (cons (list-till-elem x lis-x) (split-helper x (cdr lis) (cdr lis)))
                               (split-helper x (cdr lis) (cdr lis))))
        (else (split-helper x (cdr lis) lis-x))))

(define (split elem lis)
  (if (null? lis)
      '()
      (split-helper elem lis lis)))

(split 'c '(a b c d e))
(split '0 '(1 0 1 1 0 2 0 0 1))
(split 'x '(1 2 x 4 5))
(split 'z '(8 3 2 6 1 4))

(split '0 '(1 0 0 0 0 0 0 0 1))
(split '0 '(0 0 0 0 0 0 0))
(split '0 '(1 0 1 0 1 0 1 0 1 0 1 0 1 0))
(split '0 '(1 0 1 1 0 2 0 0))
(split '0 '(1 0 1 1 0 2 0 0 0 0))
(split '0 '(0 0 0 0 0 0 0 1 0 1 1 0 2 0 0 0 0))

;2
(display "....question 2 tests......")(newline)

(define (slice-helper l start end)
  (if (= start end)
      '()
      (cons (list-ref l start) (slice-helper l (+ start 1) end))))

(define (slice l x y)
  (cond ((null? l) '())
        ((> x y) '())
        ((< y 0) '())
        ((> x (length l)) '())
        ((= x y) '())
        ((and (< x 0)(> y (length l)) ) (slice-helper l 0 (length l)))
        ((< x 0) (slice-helper l 0 y))
        ((> y (length l)) (slice-helper l x (length l)))
        (else (slice-helper l x y))))


(slice '(0 1 2 3 4 5 6 7 8 9) 3 8)
(slice '(a b c d e f g h i j) 5 25)
(slice '(a b c d e f g) 1 3)
(slice '(0 1 2 3 4 5) -10 3)
(slice '(a b c d e f g h i j) -5 25)
(slice '(1) 0 0)
(slice '() 0 1)
(slice '(a b c d e f g h i j) 1 1)
(slice '(a b c d e f g h i j) 1 2)
(slice '(a b c d e f g h i j) 10 9)
(slice '(a b c d e f g h i j) 10 -9)
(slice '(a b c d e f g h i j) -10 -10)
(slice '(a b c d e f g h i j) -10 -9)
(slice '(a b c d e f g h i j) -10 -11)

;3
(display "....question 3 tests......")(newline)

(define (remove-helper pos amt lis)
  (cond ((null? lis)'())
        ((= amt 0) lis)
        ((= pos 0) (remove-helper pos (- amt 1) (cdr lis)))
        (else (cons (car lis) (remove-helper (- pos 1) amt (cdr lis))))))

(define (add-at-pos pos new-lis lis)
    (cond ((null? lis)new-lis)
        ((= pos 0) (append new-lis lis))
        (else (cons (car lis) (add-at-pos (- pos 1) new-lis (cdr lis))))))

(define (infuse lis x y new-lis)
  (cond ((or (< x 0) (< y 0)) lis) ;negative
        ((> x (- (length lis) 1)) lis);pos invalid
        (else (add-at-pos x new-lis (remove-helper x y lis)))))


(infuse '(a b c d e) -2 1 '(1));invalid index only return 
(infuse '(a b c d e) 2 -1 '(1)); invalid rem only returns original. 
(infuse '(a b c d e) 0 20 '(5)) ; 5?
(infuse '(a b c d e) 0 0 '())
(infuse '(a b c d e) 1 0 '())
(infuse '(a b c d e) 5 0 '(1 2)); invalid index
(infuse '() 0 0 '(1)); invalid index
(infuse '(1) 0 0 '(x y))
(infuse '(1) 0 1 '(x y))
(infuse '(1 2 3 4 5) 2 0 '(x y))
(infuse '(a b c d e) 2 1 '(x y))
(infuse '(a b c d e) 2 10 '(x y))
(infuse '(a b c d e) 0 4 '(x y))
  
;4
(display "....question 4 tests......")(newline)(newline)
(define (even-helper count size lis rev-lis)
  (if (= count size)
      '()
      (cons (+ (list-ref lis count)(list-ref rev-lis count))
            (even-helper (+ count 1) size lis rev-lis))))

(define (odd-helper count size lis rev-lis)
  (if (= count size)
      (cons (list-ref lis count) '())
      (cons (+ (list-ref lis count)(list-ref rev-lis count))
            (odd-helper (+ count 1) size lis rev-lis))))

(define (fold_once l)
  (cond ((null? l) '())
        ((= (length l) 1) l)
        ((odd? (length l)) (odd-helper 0 (- (ceiling (/ (length l) 2)) 1) l (reverse l)))
        ((even? (length l)) (even-helper 0  (ceiling (/ (length l) 2))  l (reverse l)))))

(display "even cases")(newline)
(fold_once '(6 2.5 9 1 5 7 8 4))
(fold_once '(-1 -1 -1 -1 1 1 1 1))
(fold_once '(6 2.5 9 1 5 7))
(fold_once '(6 2.5 9 1))
(fold_once '(6 2.5))
(fold_once '(6))
(fold_once '())
(newline)(display "odd cases")(newline)
(fold_once '(6 2.5 9 1 5 7 8 4 2))
(fold_once '(6 2.5 9 1 5 7 2))
(fold_once '(6 2.5 9 1 2))
(fold_once '(1 2 3.5 4 5))
(fold_once '(6 2.5 2))

;5
(display "....question 5 tests......")(newline)

(define (get_diff x)
  (if (= (length x) 1)
      '()
      (cons (- (cadr x)(car x)) (get_diff (cdr x)))))

(define (rem_neg x)
  (cond ((null? x) '())
        ((>= (car x) 0) ( cons (car x) (rem_neg (cdr x))))
        (else (rem_neg (cdr x)))))

(define (pos_diffs l)
  (if (< (length l) 2)
      '()
  (rem_neg (get_diff l))))
(pos_diffs '(6 2 9 1 5 7 8)) 
(pos_diffs '(1))
(pos_diffs '(1 2))
(pos_diffs '())
(pos_diffs '(1 2 3 4 4 6))
(pos_diffs '(1 2 3 4 5 6 7 8 9 10))
(pos_diffs '(10 9 8 7 6 5 4 3 2 1))

;6

(display "....question 6 tests......")(newline)

(define (reduce_count x count)
  (cond ((null? (cdr x)) (cons (append (list (car x)) (list count)) '()))
        ((eq? (car x) (cadr x)) (reduce_count (cdr x) (+ count 1)))
        (else (cons (append (list (car x)) (list count)) (reduce_count (cdr x) 1)))))

(define (globs l)
  (if (null? l)
      '()
        (reduce_count l 1)))

(globs '(a a g g a a a a t c))
(globs '(a b c d e g))
(globs '())
(globs '(a))
(globs '(t t t a a c g t))
(globs '(a a a a a b a a a a a b a))
(globs '(a b))

;7
(display "....question 7 tests......")(newline)

(define (sum-of-digits x) 
  (if (= x 0) 0 
      (+ (modulo x 10) 
         (sum-of-digits (/ (- x (modulo x 10)) 10)))))

(define (find-val-iter start end lis num)
  (if (= start end)
      lis
      (if (= (sum-of-digits start) num)
      (find-val-iter (+ start 1) end (append lis (list start)) num)
      (find-val-iter (+ start 1) end lis num))))

(define (all_sums x y)
  (if (or (<= x 0) (<= y 0))
      '()
      (find-val-iter (expt 10 (- x 1)) (expt 10 x) '() y)))
(all_sums 2 7)
(all_sums 3 0)
(all_sums 3 4)
(all_sums 0 0)
(all_sums 1 1)

;8
(display "....question 8 tests......")(newline)
(define (prune t)
  (if (null? t)
      '()
  (tree-filter '() t)))

(define (flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)(append (flatten (car lst)) (flatten (cdr lst))))
        (else (list lst))))

(define (tree-filter x t)
  (cond ((null? t) '())
        ((eq? x (flatten (car t))) (tree-filter x (cdr t)))
        ((list? (car t)) (cons (tree-filter x (car t)) (tree-filter x (cdr t))))
        (else (cons (car t) (tree-filter x (cdr t))))))

(prune '(1 2 () 3 4))
(prune '(a (b () (c () d))()(())(((((a b () ))))) e))
(prune '(() () () () () () () () () () () ()))
(prune '())
(prune '((((()))) ((((1)))) (((())))))
(prune '(() () (() 1 (()))))
(prune '(() () () ((((())))) (()((()))()) 1 (2 ())))

;9
(display "....question 9 tests......")(newline)
(display "test more")(newline)

(define (min-helper bef curr min count lis)
  (cond ((and (null? lis) (< min curr)) (cons (- count 1) '()))
        ((and (null? lis) (< curr 0) (< curr bef)) (cons count '()));here
        ((null? lis) '())
        ((< curr bef) (min-helper curr (car lis) curr (+ count 1 ) (cdr lis)))
        ((< min curr) (cons (- count 1)
                            (min-helper curr (car lis) (car lis) (+ count 1) (cdr lis))))
        (else (min-helper curr (car lis) (car lis) (+ count 1) (cdr lis)))
        ))

(define (minima lis)
  (if (null? lis)
      '()
  (min-helper 0 (car lis) (car lis) 0 (cdr lis))))

(minima '(1 5 9 3 5 4 8))
(minima '(-42 25 -1))
(minima '(0 1 0 0 0 -1 1 1 -1 1 0 1))
(minima '(6 4 2 1 3 5))
(minima '(-1))
(minima '(0))
(minima '(1))
(minima '())
(minima '(1 2 3 4 5 6 7 8 9 10))
(minima '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10))
(minima '(10 9 8 7 6 5 4 3 2 1))
(minima '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1)); should be (0)
(minima '(-1 1 -1 -1 1 -1 0 -1 -1 0 -2))

;Part b

(define (make-list pred mute start end step)
  (cond ((= step 0) '())
        ((and (negative? step) (< start end)) '())
        ((and (> step 0)(> start end)) '())
        ((pred start)(cons (mute start) (make-list pred mute (+ start step) end step)))
        (else (make-list pred mute (+ start step) end step))))

(define (list-generator pred mute)
  (lambda (x y z)  (make-list pred mute x y z)))

((list-generator odd? (lambda(x)(* x 2))) 1 20 3)

(define f (list-generator (lambda(x)#t) (lambda(x)x)))
(f 0 10 2)
(f 1 1 1)
(f 1 1 0)
((list-generator (lambda(x)#t)(lambda(x)x)) 16 0 -4)
((list-generator even? (lambda(x) x*x)) 0 20 -4)
((list-generator even? (lambda(x) x*x)) 20 0 4)
((list-generator (lambda(x)#f) (lambda(x) x*x)) 20 0 4)
((list-generator (lambda(x)#t)(lambda(x)x)) 16 0 -1)
((list-generator (lambda(x)#t)(lambda(x)x)) 2 2 1)
((list-generator (lambda(x)#t)(lambda(x)x)) -1 1 1)
((list-generator (lambda(x)#t)(lambda(x)x)) 2 10 0)

;Part c
(define (eval-prefix tree)
  (if (number? tree)
      tree
      ((get-operation (car tree)) (eval-prefix (cadr tree))
                     (eval-prefix (caddr tree)))))
                      
(define (get-operation x)
  (cond ((eq? x '+) (lambda (x y) (+ x y)))
        ((eq? x '-)(lambda (x y) (- x y)))
        ((eq? x '/)(lambda (x y) (/ x y)))
        ((eq? x '*)(lambda (x y) (* x y)))))


(eval-prefix '(* (+ 1 2) (* 1 (+ 1 (- 4 2)))))
(eval-prefix '(+ (+ 1 2) 3))
(eval-prefix '(+ (+ 1 2) (* 3 (/ 3 1))))

