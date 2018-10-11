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
    substitute-tests
    (check-equal?
     (substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)
     '(she loves you maybe maybe maybe))
    (check-equal?
     (substitute '(the fork and the spoon) 'the 'a)
     '(a fork and a spoon))
    (check-equal? (substitute '(where are you) 'where 'who) '(who are you)))
   (define/test
    f-tests
    (check-not-exn (lambda () f1) "f1 did not work")
    (check-not-exn (lambda () (f2)) "(f2) did not work")
    (check-not-exn (lambda () (f3 3)) "(f3 3) did not work")
    (check-not-exn (lambda () ((f4))) "((f4)) did not work")
    (check-not-exn (lambda () (((f5)) 3)) "(((f5)) 3)) did not work"))
   (define/test
    make-tester-tests
    (check-equal? ((make-tester 'hal) 'hal) #t)
    (check-equal? ((make-tester 'hal) 'cs61as) #f)
    (define sicp-author-and-astronomer? (make-tester 'gerry))
    (check-equal? (sicp-author-and-astronomer? 'hal) #f)
    (check-equal? (sicp-author-and-astronomer? 'gerry) #t)
    (check-equal? ((make-tester 'penny) 'quarter) #f)
    (check-equal? ((make-tester 'penny) 'penny) #t))
   (define/test
    product-tests
    (check-equal? (product add1 4 add1 6) 210)
    (check-equal? (product (lambda (x) (- x 1)) 2 (lambda (x) (* x x)) 20) 45)
    (check-equal? (product (lambda (x) x) 4 add1 6) 120)
    (check-equal? (factorial 5) 120)
    (check-equal? (factorial 3) 6)
    (check-= (estimate-pi) 3.1416 0.05))
   (define/test
    my-accumulate-tests
    (check-equal?
     (my-accumulate + 3 (lambda (x) x) 4 (lambda (x) (* 2 x)) 20)
     31)
    (check member
     (my-accumulate se '() (lambda (x) (* x x)) 1 add1 7)
     '((1 4 9 16 25 36 49) (49 36 25 16 9 4 1)))
    (check-equal? (my-accumulate * 1 add1 3 add1 5) 120))
   (define/test
    filtered-accumulate-tests
    (check member
     (filtered-accumulate se '() (lambda (x) (* x x)) 1 add1 7 odd?)
     '((49 25 9 1) (1 9 25 49)))
    (check-equal?
     (filtered-accumulate
      +
      0
      (lambda (x) (* 3 x))
      3
      add1
      17
      (lambda (x) (= (remainder x 5) 0)))
     90)
    (check-equal? (filtered-accumulate * 1 add1 2 add1 6 even?) 105)
    (check-equal? (sum-sq-prime 4 8) 74)
    (check-equal? (sum-sq-prime 10 20) 940)
    (check-equal? (prod-of-some-numbers 6) 5)
    (check-equal? (prod-of-some-numbers 8) 105)
    (check-equal? (prod-of-some-numbers 7) 720))
   (define/test
    cubic-tests
    (check-equal? ((cubic 1 1 1) 1) 4)
    (check-equal? ((cubic 5 4 3) 2) 39)
    (check-equal? ((cubic 2 2 2) 2) 22))
   (define/test
    double-tests
    (check-equal? ((double add1) 3) 5)
    (check-equal? ((double (lambda (x) (* x x))) 3) 81)
    (check-equal? ((double (lambda (x) (/ x 2))) 64) 16))
   (define/test
    repeated-tests
    (check-equal? ((my-repeated (lambda (x) (* x x)) 2) 5) 625)
    (check-equal? ((my-repeated add1 5) 12) 17)
    (check-equal? ((my-repeated (lambda (x) (/ x 2)) 4) 32) 2))
   (define/test
    my-every-tests
    (check-equal? (my-every (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16))
    (check-equal? (my-every first '(nowhere man)) '(n m))
    (check-equal? (my-every last '()) '())
    (check-equal? (my-every add1 '(56 2 9)) '(57 3 10)))
   all-names)

