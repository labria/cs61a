; Exercise 1 - Define substitute

(define (substitute sent old-word new-word)
  ; Your code here
  (error "Not yet implemented")
)


; Exercise 2 - Try out the expressions!


(lambda (x) (+ x 3))
;-> returns:

((lambda (x) (+ x 3)) 7)
;-> returns:

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
;-> returns:

(define plus3 (make-adder 3)) 
(plus3 7)
;-> returns:

(define (square x) (* x x)) 
(square 5)
;-> returns:

(define sq (lambda (x) (* x x))) 
(sq 5)
;-> returns

(define (try f) (f 3 5)) 
(try +)
;-> returns:

(try word)
;-> returns:



; Exercise 3
#|

How many arguments g has: 

Type of value returned by g:

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5

; Exercise 5 - Try out the expressions

(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t 1+) 0) returns:

2. ((t (t 1+)) 0) returns:

3. (((t t) 1+) 0) returns:

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns:

2. ((t (t s)) 0) returns:

3. (((t t) s) 0) returns:

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 8 - SICP exercises

; SICP 1.31a

(define (product term a next b)
  ; Your code here
  (error "Not yet implemented")
)

(define (estimate-pi)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.32a

(define (accumulate combiner null-value term a next b)
  ; Your code here
  (error "Not yet implemented")
)

#|

Write sum in terms of accumulate:


Write product in terms of accumulate:

|#

; SICP 1.33

(define (filtered-accumulate combiner null-value term a next b pred)
  ; Your code here
  (error "Not yet implemented")
)

(define (prime? n)
  (define (loop i)
    (cond ((= i n) #t)
          ((= (remainder n i) 0) #f)
          (else (loop (+ i 1)))))
  (if (<= n 1)
      #f
      (loop 2)))

(define (sum-sq-prime a b)
  ; Your code here
  (error "Not yet implemented")
)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.40 - Define cubic

(define (cubic a b c)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.41 - Define double

(define (double proc)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.43 - Define repeated

(define (repeated proc n)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 9 - Define every

(define (every proc sent)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns:

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns:

(keep even? '(781 5 76 909 24))
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns:

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns:
|#
