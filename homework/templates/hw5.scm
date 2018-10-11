;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out


; Exercise 2 Mobile
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

; b. Define total-weight.

; c. Define balanced?

; d. Redefine all the necessary procedures to work with the new
; constructors given below.

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))


;Exercise 3a - Define square-tree


;Exercise 3b - Define tree-map



;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init ??)
	    (accumulate-n op init ??))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map ?? m))

(define (transpose mat)
  (accumulate-n ?? ?? mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map ?? m)))


;Exercise 6 - Give the property that op should satisfy:
#|
Your property here
|#

;Exercise 7 - Define equal?



;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map ?? rest)))))


;Exercuse 9 - Modify the calc program

;; Scheme calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + 0 (cdr args))))))
	((eq? fn '*) (accumulate * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate * 1 (cdr args))))))
	(else (error "Calc: bad operator:" fn))))
