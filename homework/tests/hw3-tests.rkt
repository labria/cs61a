#lang racket

(require racket/sandbox)

(provide (all-defined-out))

(define (create-tests e questions)
   (define all-names '())
   (define-syntax-rule
    (define/test name code ...)
    (when (or (null? questions) (member (symbol->string 'name) questions))
      (set! all-names (append all-names (list (symbol->string 'name))))
      (e
       '(define name
          (test-suite
           (symbol->string 'name)
           (test-case (symbol->string 'name) code ...))))
      (e '(hash-set! test-map (symbol->string 'name) name))))
   (define/test
    fast-expt-iter-tests
    (check-equal? (fast-expt-iter 2 0) 1)
    (check-equal? (fast-expt-iter 2 3) 8)
    (check-equal? (fast-expt-iter 2 4) 16)
    (check-equal? (fast-expt-iter 2 10) 1024)
    (check-equal? (fast-expt-iter 3 2) 9)
    (check-equal? (fast-expt-iter 3 3) 27)
    (check-equal? (fast-expt-iter 25 2) 625))
   ;;(define/test
     ;;phi-tests
     ;;(check-= (phi 10) 1.618 0.001)
     ;;(check-= (- (phi 10) (phi 11)) 0 0.001)
     ;;(check > (phi 10) (phi 11))
     ;;(check-= (phi) 1.618 0.001))
   (define/test
    cont-frac-tests
    (check-equal? (cont-frac (lambda (x) x) (lambda (x) x) 1) 1)
    (check-= (cont-frac (lambda (x) x) (lambda (x) x) 10) 0.5819 0.001)
    (check-equal?
     (<
      (cont-frac (lambda (x) x) (lambda (x) x) 12)
      (cont-frac (lambda (x) x) (lambda (x) x) 13))
     #t)
    (check-= (cont-frac (lambda (x) x) (lambda (x) (* 2 x)) 5) 0.4085 0.001)
    (check-= (cont-frac (lambda (x) (* 2 x)) (lambda (x) x) 5) 0.9167 0.001)
    (check-= (e 5) 2.7183 0.001)
    (check-= (e 10) 2.71828 0.0001)
    (check-=
     (cont-frac (lambda (x) (* 2 x)) (lambda (x) (* 2 x)) 5)
     0.7089
     0.001)
    (check-= (e 20) 2.7182818 0.000001))
   (define/test
    next-perf-tests
    (check-equal? (next-perf 0) 6)
    (check-equal? (next-perf 7) 28)
    (check-equal? (next-perf 8) 28)
    (check-equal? (next-perf 29) 496)
    (check-equal? (next-perf 20) 28))
   all-names)

