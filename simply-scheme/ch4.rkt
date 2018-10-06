#lang racket
(require berkeley)

;; Exercise 4.5

(define (F-C f-temp)
  (* (/ 5 9) (- f-temp 32)))

(define (C-F c-temp)
  (+ (* (/ 9 5) c-temp) 32))

;; Exercise 4.6

(define (fourth x)
  (* x x x x))

(define (fourth1 x)
  (square (square x)))

;; Exercise 4.7

(define (abs x)
  (sqrt (square x)))

;; Exercise 4.8

(define (scientific x y)
  (* x (expt 10 y)))

;; Exercise 4.9

(define (discount x y)
  (* x (/ (- 100 y) 100)))

;; Exercise 4.10

(define (tip x)
  (- (ceiling (* x 1.15)) x))