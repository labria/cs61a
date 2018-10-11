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
    interval-selectors-tests
    (check-equal? (upper-bound (make-interval 10 12)) 12)
    (check-equal? (lower-bound (make-interval 10 12)) 10)
    (check-equal? (upper-bound (make-interval 32 212)) 212)
    (check-equal? (lower-bound (make-interval 32 212)) 32))
   (define/test
    sub-interval-tests
    (check-equal?
     (let ((res (sub-interval (make-interval 20 30) (make-interval 10 12))))
       (list (lower-bound res) (upper-bound res)))
     '(8 20))
    (check-equal?
     (let ((res (sub-interval (make-interval 63 67) (make-interval 21 31))))
       (list (lower-bound res) (upper-bound res)))
     '(32 46)))
   (define/test
    spans-zero-tests
    (define (list-equal? x y)
      (define (item-equal? a b)
        (if (and (number? a) (number? b))
            (< (abs (- a b)) 0.0001)
            (equal? a b)))
      (andmap item-equal? x y))
    (check
     list-equal?
     (let ((res (div-interval (make-interval -5 5) (make-interval 10 20))))
       (list (lower-bound res) (upper-bound res)))
     '(-0.5 0.5))
    (check-exn
     exn:fail?
     (lambda () (div-interval (make-interval 10 20) (make-interval -5 5)))))
   (define/test
    make-center-percent-tests
    (define (list-equal? x y)
      (define (item-equal? a b)
        (if (and (number? a) (number? b))
            (< (abs (- a b)) 0.0001)
            (equal? a b)))
      (andmap item-equal? x y))
    (check
     list-equal?
     (let ((res
            (add-interval (make-center-percent 20 5) (make-interval 6 14))))
       (list (lower-bound res) (upper-bound res) (center res) (percent res)))
     `(25 35 30 ,(/ 50 3)))
    (check
     list-equal?
     (let ((res
            (sub-interval
             (make-center-percent 40 10)
             (make-center-percent 4 25))))
       (list (lower-bound res) (upper-bound res) (center res) (percent res)))
     `(31 41 36 ,(/ 500 36))))
   (define/test
    last-pair-tests
    (check-equal? (last-pair (list 23 72 149 34)) '(34))
    (check-equal? (last-pair (list 'hello)) '(hello))
    (check-equal? (last-pair '((1 2) (3 4))) '((3 4)))
    (check-equal? (last-pair '(a b c d)) '(d)))
   (define/test
    same-parity-tests
    (check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
    (check-equal? (same-parity 2 4 3 7 6 5) '(2 4 6))
    (check-equal? (same-parity 4) '(4))
    (check-equal? (same-parity 4 3 5 7 13) '(4))
    (check-equal? (same-parity 1 4 5 2 9 7) '(1 5 9 7))
    (check-equal? (same-parity 6 4 9 2 5 7) '(6 4 2)))
   (define/test
    substitute-tests
    (check-equal?
     (substitute
      '((lead guitar) (bass guitar) (rhythm guitar) drums)
      'guitar
      'axe)
     '((lead axe) (bass axe) (rhythm axe) drums))
    (check-equal?
     (substitute '(teh fork and teh spoon) 'teh 'the)
     '(the fork and the spoon))
    (check-equal? (substitute '(drums) 'guitar 'axe) '(drums))
    (check-equal?
     (substitute '(((sub here) sub sub here) (((sub)))) 'sub 'dub)
     '(((dub here) dub dub here) (((dub)))))
    (check-equal?
     (substitute '((((a nested list)))) 'nested 'deep)
     '((((a deep list))))))
   (define/test
    substitute2-tests
    (check-equal?
     (substitute2
      '((4 calling birds) (3 french hens) (2 turtle doves))
      '(1 2 3 4)
      '(one two three four))
     '((four calling birds) (three french hens) (two turtle doves)))
    (check-equal?
     (substitute2 '(i think i lost my love) '(i my) '(you your))
     '(you think you lost your love))
    (check-equal?
     (substitute2
      '((((1) four 2) (3 2 four)) 5)
      '(1 2 3 four 5)
      '(one two three 4 five))
     '((((one) 4 two) (three two 4)) five))
    (check-equal?
     (substitute2
      '((apple) ((banana)) cherry)
      '(apple cherry)
      '(chocolate split))
     '((chocolate) ((banana)) split)))
   all-names)

