#lang sicp

(define (search f a b)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average a b)))
    (if (close-enough? a b)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f a midpoint))
                ((negative? test-value)
                 (search f midpoint b))
                (else midpoint))))))

(define (average x y)
  (/ (+ x y) 2))