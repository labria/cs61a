#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter b n)
  ; Your code here
  (error "Not yet implemented")
)

; Exericse 2 - Define phi

(define (phi)
  ; Your code here
  (error "Not yet implemented")
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
  ; Your code here
  (error "Not yet implemented")
)

;; Iterative version
(define (cont-frac-iter n d k)
  ; Your code here
  (error "Not yet implemented")
)

(define (e k)
  ; Your code here to estimate e using cont-frac with k terms.
  (error "Not yet implemented")
)

; Exercise 4 - Define next-perf

(define (next-perf n)
  ;  Your code here
  (error "Not yet implemented")
)

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter:

|#
