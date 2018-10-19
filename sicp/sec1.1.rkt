#lang sicp

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; Exercise 1.1

#|

10                                  ;10
(+ 5 3 4)                           ;12
(- 9 1)                             ;8
(/ 6 2)                             ;3
(+ (* 2 4) (- 4 6))                 ;6
(define a 3)                        ;nothing, a = 3
(define b (+ a 1))                  ;nothing, b = 4
(+ a b (* a b))                     ;19
(= a b)                             ;#f
(if (and (> b a) (< b (* a b)))
    b
    a)                              ;4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))                    ;16
(+ 2 (if (> b a) b a))              ;6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))                         ;16

|#

;; Exercise 1.2

#|

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

|#

;; Exercise 1.3
(define (two-sum x y z)
  (cond ((and (>= x z) (>= y z))
         (sum-of-squares x y))
        ((and (>= x y) (>= z y))
         (sum-of-squares x z))
        (else
         (sum-of-squares y z))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0)            ; if b is positive
       +                  ; then the operator is + and the procedure simplifies to (+ a b)
       -) a b))           ; otherwise the operator is - and the procedure simplifies to (- a b)

;; Exercise 1.5

#|

Applicative Order: When (test 0 (p)) is called, the infinite recursive function (p) will not be called. The result of the
if is 0, since the predicate, (= x 0) is true.

Normal Order: (p) will be evaluated as soon as (test 0 (p)) is called, resulting in an infinite loop, even though the predicate is
true.

|#

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6

#|

Since if is a special form that does not evaluate the alternative expression, the recursive call only executes when the predicate
is true. In Alyssa's new-if procedure is a normal procedure and the recursive call will always be evaluated. This results in an
infinite loop and the square root is never returned.

|#