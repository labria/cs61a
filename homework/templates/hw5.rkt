#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.


; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (error "Not yet implemented"))

(define (right-branch mobile)
  (error "Not yet implemented"))

(define (branch-structure branch)
  (error "Not yet implemented"))

; b. Define total-weight.

(define (total-weight mobile)
  (error "Not yet implemented"))

; c. Define balanced?

(define (balanced? b-mobile)
  (error "Not yet implemented"))

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))


;Exercise 3a - Define square-tree

(define (square-tree d-l)
  (error "Not yet implemented"))

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
  (error "Not yet implemented"))

;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init "YOUR CODE HERE")
	    (accumulate-n op init "YOUR CODE HERE"))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map "YOUR CODE HERE" m))

(define (transpose mat)
  (accumulate-n "YOUR CODE HERE" "YOUR CODE HERE" mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map "YOUR CODE HERE" m)))


;Exercise 6 - Give the property that op should satisfy:

#|

Your property here

|#

;Exercise 7 - Define equal?

(define (my-equal? l1 l2)
  (error "Not yet implemented"))

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map "YOUR CODE HERE" rest)))))


;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (foldr + 0 (cdr args))))))
	((eq? fn '*) (foldr * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (foldr * 1 (cdr args))))))
	(else (error "Calc: bad operator:" fn))))
