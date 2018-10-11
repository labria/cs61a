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
    mobile-selectors-tests
    (check-equal?
     (let ((m (make-mobile (make-branch 2 3) (make-branch 1 4))))
       (let ((left (left-branch m)) (right (right-branch m)))
         (list
          (branch-length left)
          (branch-length right)
          (branch-structure left)
          (branch-structure right))))
     '(2 1 3 4))
    (check-equal?
     (let ((m (make-mobile (make-branch 5 6) (make-branch 7 8))))
       (let ((left (left-branch m)) (right (right-branch m)))
         (list
          (branch-length left)
          (branch-structure left)
          (branch-length right)
          (branch-structure right))))
     '(5 6 7 8)))
   (define/test
    total-weight-tests
    (check-equal?
     (total-weight (make-mobile (make-branch 2 3) (make-branch 1 4)))
     7)
    (check-equal?
     (total-weight
      (make-mobile
       (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 2 9)))
       (make-branch 6 1)))
     15)
    (check-equal?
     (total-weight (make-mobile (make-branch 5 6) (make-branch 10 13)))
     19))
   (define/test
    balanced?-tests
    (check-true (balanced? (make-mobile (make-branch 3 6) (make-branch 9 2))))
    (check-false (balanced? (make-mobile (make-branch 2 5) (make-branch 3 3))))
    (check-true
     (balanced?
      (make-mobile
       (make-branch 3 (make-mobile (make-branch 4 6) (make-branch 3 8)))
       (make-branch 6 7))))
    (check-false
     (balanced?
      (make-mobile
       (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 3 9)))
       (make-branch 6 7))))
    (check-true
     (balanced? (make-mobile (make-branch 10 20) (make-branch 25 8))))
    (check-false
     (balanced? (make-mobile (make-branch 10 2) (make-branch 25 8)))))
   (define/test
    square-tree-tests
    (check-equal? (square-tree '()) '())
    (check-equal? (square-tree '(2 5 (3))) '(4 25 (9)))
    (check-equal?
     (square-tree '(1 (2 (3 4) 5) (6 7)))
     '(1 (4 (9 16) 25) (36 49)))
    (check-equal? (square-tree '(3 (6 ((4) 5)))) '(9 (36 ((16) 25)))))
   (define/test
    tree-map-tests
    (check-equal?
     (tree-map (lambda (x) (* x x)) '(1 (2 (3 4) 5) (6 7)))
     '(1 (4 (9 16) 25) (36 49)))
    (check-equal?
     (tree-map first '(77 (((violet indigo)) blue (green yellow)) orange red))
     '(7 (((v i)) b (g y)) o r))
    (check-equal?
     (tree-map (lambda (x) (if x x 'false)) '(1 (#f) two #f))
     '(1 (false) two false))
    (check-equal? (tree-map bf '((the (hate)) boils)) '((he (ate)) oils)))
   (define/test
    accumulate-n-tests
    (check-equal?
     (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
     '(22 26 30))
    (check-equal?
     (accumulate-n word "" '((t f c s) (h a a a) (e t t t)))
     '(the fat cat sat))
    (check-equal? (accumulate-n * 2 '((3))) '(6))
    (check-equal? (accumulate-n + 0 '((3 2) (8 10) (1 1) (5 6))) '(17 19)))
   (define/test
    matrix-*-vector-tests
    (check-equal?
     (matrix-*-vector '((1 2 3 4) (5 6 7 8) (9 10 11 12)) '(2 3 4 5))
     '(40 96 152))
    (check-equal?
     (matrix-*-vector '((1 2) (3 4) (5 6) (7 8)) '(1 2))
     '(5 11 17 23)))
   (define/test
    transpose-tests
    (check-equal?
     (transpose '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
     '((1 5 9) (2 6 10) (3 7 11) (4 8 12)))
    (check-equal? (transpose '((10 12 4) (3 9 8))) '((10 3) (12 9) (4 8))))
   (define/test
    matrix-*-matrix-tests
    (check-equal?
     (matrix-*-matrix '((1 2 3) (4 5 6)) '((2 3 4) (5 6 7) (2 4 6)))
     '((18 27 36) (45 66 87)))
    (check-equal?
     (matrix-*-matrix '((1 2) (3 4)) '((5 7) (6 8)))
     '((17 23) (39 53))))
   (define/test
    my-equal?-tests
    (check-false (my-equal? 'foo '(foo)))
    (check-true (my-equal? 'foo 'foo))
    (check-true (my-equal? '(foo) '(foo)))
    (check-false (my-equal? 'foo 'bar))
    (check-true (my-equal? '(this is a list) '(this is a list)))
    (check-false (my-equal? '(this is a list) '(this (is a) list)))
    (check-true (my-equal? '(this (is a) list) '(this (is a) list)))
    (check-true (my-equal? '((((this (is a) list)))) '((((this (is a) list))))))
    (check-false (my-equal? '((((this (is a) list)))) '((((this (is) list))))))
    (check-false (my-equal? 'word '(word)))
    (check-false (my-equal? 'word 'symbol))
    (check-false (my-equal? '(a (nested) list) '(a (deep) list)))
    (check-true (my-equal? '(a (nested) list) '(a (nested) list))))
   (define/test
    subsets-tests
    (define (permute-equal? x y)
      (define (item-equal? a b)
        (if (and (number? a) (number? b)) (= a b) (equal? a b)))
      (andmap item-equal? x y))
    (check
     permute-equal?
     (subsets '(1 2 3))
     '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
    (check-equal? (subsets '()) '(()))
    (check permute-equal? (subsets '(hi)) '(() (hi)))
    (check permute-equal? (subsets '(5 8)) '(() (8) (5) (5 8))))
   (define/test
    calc-tests
    (check-equal? (calc-eval '(first foo)) 'f)
    (check-equal? (calc-eval 'foo) 'foo)
    (check-equal? (calc-eval '(first (butfirst hello))) 'e)
    (check-equal? (calc-eval '(word (bl penny) ies)) 'pennies)
    (check-equal? (calc-eval '(last (butlast flower))) 'e)
    (check-equal? (calc-eval '(+ 2 (word (first 987) (bf 56)))) 98)
    (check-equal? (calc-eval '(word (first butter) (* 2 (last 52)))) 'b4))
   all-names)

