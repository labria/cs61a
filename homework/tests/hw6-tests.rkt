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
    make-from-mag-ang-tests
    (check-= ((make-from-mag-ang-mp (sqrt 2) (atan 1)) 'real-val) 1 0.001)
    (check-= ((make-from-mag-ang-mp (sqrt 2) (atan 1)) 'imag-val) 1 0.001)
    (check-= ((make-from-mag-ang-mp (sqrt 2) (atan 1)) 'ang) (atan 1) 0.001)
    (check-=
     ((make-from-mag-ang-mp (sqrt 2) (atan 1)) 'mag)
     (sqrt 2)
     0.001)
    (check-= ((make-from-mag-ang-mp 2.3 4) 'real-val) -1.5034 0.001)
    (check-= ((make-from-mag-ang-mp 2.3 4) 'imag-val) -1.7406 0.001)
    (check-= ((make-from-mag-ang-mp 2.3 4) 'ang) 4 0.001)
    (check-= ((make-from-mag-ang-mp 2.3 4) 'mag) 2.3 0.001))
   (define/test
    equ?-tests
    (check-true (equ? (make-racket-number 2) (make-racket-number 2)))
    (check-false (equ? (make-racket-number 2) (make-racket-number 3)))
    (check-true (equ? (make-rational 4 6) (make-rational 10 15)))
    (check-false (equ? (make-rational 4 6) (make-rational 12 15)))
    (check-true
     (equ? (make-complex-from-real-imag 3 0) (make-complex-from-mag-ang 3 0)))
    (check-false
     (equ?
      (make-complex-from-real-imag 1.0 1.0)
      (make-complex-from-mag-ang (sqrt 2) (/ 3.14 4)))))
   (define/test
    =zero?-tests
    (check-false (=zero? (make-racket-number 2)))
    (check-true (=zero? (make-racket-number 0)))
    (check-true (=zero? (make-rational 0 100)))
    (check-false (=zero? (make-rational 1 1000)))
    (check-true (=zero? (make-complex-from-real-imag 0 0)))
    (check-false (=zero? (make-complex-from-real-imag 0 1)))
    (check-true (=zero? (make-complex-from-mag-ang 0 2)))
    (check-false (=zero? (make-complex-from-mag-ang 2 0))))
   (define/test
    same-type-coercion-tests
    (check-exn
     exn:fail?
     (lambda ()
       (apply-generic-better-coercion
        'non-existent
        (make-racket-number 3)
        (make-racket-number 4))))
    (check-exn
     exn:fail?
     (lambda ()
       (apply-generic-better-coercion
        'nonsense
        (make-rational 1 2)
        (make-rational 3 4)))))
   (define/test
    raise-num-tests
    (check-true (equal? (raise-num (make-integer 4)) (make-rational 4 1)))
    (check-true
     (equal? (raise-num (make-rational -8 6)) (make-racket-number (/ -4 3))))
    (check-equal?
     (let ((x (contents (raise-num (make-racket-number 23.4)))))
       (list (real-val x) (imag-val x)))
     '(23.4 0)))
   (define/test
    map-1-tests
    (check-equal? (eval-1 '(map-1 (lambda (x) (* x x)) '(8 2 11))) '(64 4 121))
    (check-equal? (eval-1 '(map-1 car '((1 2 3) (1 2 3)))) '(1 1))
    (check-equal? (eval-1 '(map-1 cdr '((1 2 3) (1 2 3)))) '((2 3) (2 3)))
    (check-equal?
     (eval-1 '(map-1 (lambda (x) (foldl + 0 x)) '((1 2 3) (1 2 3))))
     '(6 6)))
   (define/test
    let-tests
    (check-equal? (eval-1 '(let ((x 1) (y 2)) (+ x y))) 3)
    (check-equal?
     (eval-1
      '(let ((pi 3.1416) (r 10))
         (let ((circumference (* 2 pi r)) (area (* pi r r)))
           (list circumference area))))
     '(62.832 314.16)))
   (define/test
    union-ordered-set-tests
    (check-equal? (union-ordered-set '(1 3) '(1 2 4)) '(1 2 3 4))
    (check-equal? (union-ordered-set '(1 2 3 4) '(1 2 3 4 5)) '(1 2 3 4 5))
    (check-equal? (union-ordered-set '(5 10) '(1 2 8 11)) '(1 2 5 8 10 11)))
   (define/test
    adjoin-set-tests
    (check-equal?
     tree1
     (make-binary-tree
      7
      (make-binary-tree 3 (make-binary-tree 1 '() '()) (make-binary-tree 5 '() '()))
      (make-binary-tree 9 '() (make-binary-tree 11 '() '()))))
    (check-equal?
     tree2
     (make-binary-tree
      3
      (make-binary-tree 1 '() '())
      (make-binary-tree
       7
       (make-binary-tree 5 '() '())
       (make-binary-tree 9 '() (make-binary-tree 11 '() '())))))
    (check-equal?
     tree3
     (make-binary-tree
      5
      (make-binary-tree 3 (make-binary-tree 1 '() '()) '())
      (make-binary-tree 9 (make-binary-tree 7 '() '()) (make-binary-tree 11 '() '())))))
   all-names)

