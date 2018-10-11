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
    sum-of-squares-tests
    (check-equal? (sum-of-squares 3 4) 25)
    (check-equal? (sum-of-squares -1 -2) 5)
    (check-equal? (sum-of-squares 1 2) 5))
   (define/test
    can-drive-tests
    (check-equal? (can-drive 10) '(Not yet))
    (check-equal? (can-drive 15) '(Not yet))
    (check-equal? (can-drive 16) '(Good to go))
    (check-equal? (can-drive 20) '(Good to go))
    (check-equal? (can-drive 18) '(Good to go)))
   (define/test
    fizzbuzz-tests
    (check-equal? (fizzbuzz 6) 'fizz)
    (check-equal? (fizzbuzz 10) 'buzz)
    (check-equal? (fizzbuzz 45) 'fizzbuzz)
    (check-equal? (fizzbuzz 0) 'fizzbuzz)
    (check-equal? (fizzbuzz 1) 1)
    (check-equal? (fizzbuzz 293) 293)
    (check-equal? (fizzbuzz 9) 'fizz)
    (check-equal? (fizzbuzz 100) 'buzz)
    (check-equal? (fizzbuzz 15) 'fizzbuzz)
    (check-equal? (fizzbuzz 101) 101))
   all-names)

