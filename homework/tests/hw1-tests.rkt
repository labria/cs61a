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
    dupls-removed-tests
    (define (permute-equal? x y)
      ;; Assumption: the expected list y has no duplicates
      ;; Then x is a permutation of y iff they are the same length and
      ;; every element of y is in x.
      (and (andmap (lambda (y-elem) (member y-elem x)) y)
	   (= (length x) (length y))))
    (check permute-equal? (dupls-removed '(a b c a e d e b)) '(c a d e b))
    (check permute-equal?
     (dupls-removed '(apple banana carrot))
     '(apple banana carrot))
    (check permute-equal? (dupls-removed '(a a a b a b a a)) '(b a))
    (check permute-equal?
     (dupls-removed '(the fork and the spoon))
     '(fork and the spoon)))

   (define/test
    count-word-tests
    (check-equal? (count-word '(i really really like scheme) 'really) 2)
    (check-equal? (count-word '(i lambda scheme) 'love) 0)
    (check-equal? (count-word '(the lion the witch and the wardrobe) 'the) 3)
    (check-equal? (count-word '(once upon a time) 'upon) 1))

   (define/test
    squares-tests
    (check-equal? (squares '(1 2 3)) '(1 4 9))
    (check-equal? (squares '(8)) '(64))
    (check-equal? (squares '(5 10)) '(25 100)))

   (define/test
    switch-tests
    (check-equal?
     (switch '(you told me that I should wake you up))
     '(I told you that you should wake me up))
    (check-equal? (switch '(the rain in spain)) '(the rain in spain))
    (check-equal? (switch '(she loves you)) '(she loves me))
    (check-equal? (switch '(I love her)) '(you love her))
    (check-equal?
     (switch '(you figured out that I hate you))
     '(I figured out that you hate me))
    (check-equal? (switch '(I like you)) '(you like me)))

   (define/test
    ordered?-tests
    (check-equal? (ordered? '(1 2 3)) #t)
    (check-equal? (ordered? '(2 1 3)) #f)
    (check-equal? (ordered? '(2)) #t)
    (check-equal? (ordered? '(4 9 10)) #t)
    (check-equal? (ordered? '(5 4)) #f))

   (define/test
    ends-e-tests
    (check-equal?
     (ends-e '(please put the salami above the blue elephant))
     '(please the above the blue))
    (check-equal? (ends-e '(nothing ends with it)) '())
    (check-equal? (ends-e '(where is the place)) '(where the place)))

   all-names)

