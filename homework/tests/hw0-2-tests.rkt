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
    first-two-tests
    (check-equal? (first-two 'ambulatory) 'am)
    (check-equal? (first-two 'thursday) 'th))
   (define/test
    two-first-tests
    (check-equal? (two-first 'brian 'epstein) 'be)
    (check-equal? (two-first 'abcde 'fghij) 'af))
   (define/test
    two-first-sent-tests
    (check-equal? (two-first-sent '(brian epstein)) 'be)
    (check-equal? (two-first-sent '(abcde fghij)) 'af))
   (define/test
    teen?-tests
    (check-equal? (teen? 13) #t)
    (check-equal? (teen? 19) #t)
    (check-equal? (teen? 12) #f)
    (check-equal? (teen? 20) #f)
    (check-equal? (teen? 16) #t)
    (check-equal? (teen? 100) #f))
   (define/test
    indef-article-tests
    (check-equal? (indef-article 'beatle) '(a beatle))
    (check-equal? (indef-article 'album) '(an album))
    (check-equal? (indef-article 'ox) '(an ox))
    (check-equal? (indef-article 'yak) '(a yak)))
   (define/test
    insert-and-tests
    (check-equal?
     (insert-and '(john bill wayne fred joey))
     '(john bill wayne fred and joey))
    (check-equal?
     (insert-and '(apples bananas oranges))
     '(apples bananas and oranges)))
   (define/test
    query-tests
    (check-equal? (query '(you are experienced)) '(are you experienced?))
    (check-equal?
     (query '(i should have known better))
     '(should i have known better?)))
   (define/test
    european-time-tests
    (check-equal? (european-time '(8 am)) 8)
    (check-equal? (european-time '(4 pm)) 16)
    (check-equal? (european-time '(12 am)) 0)
    (check-equal? (european-time '(12 pm)) 12)
    (check-equal? (european-time '(7 pm)) 19))
   (define/test
    american-time-tests
    (check-equal? (american-time 8) '(8 am))
    (check-equal? (american-time 16) '(4 pm))
    (check-equal? (american-time 0) '(12 am))
    (check-equal? (american-time 12) '(12 pm))
    (check-equal? (american-time 19) '(7 pm)))
   (define/test
    describe-time-tests
    (define (sent-equal? x y)
      (define (item-equal? a b)
        (if (and (number? a) (number? b))
            (< (abs (- a b)) 0.0001)
            (equal? a b)))
      (andmap item-equal? x y))
    (check sent-equal? (describe-time 48) '(48 seconds))
    (check sent-equal? (describe-time 150) '(2.5 minutes))
    (check sent-equal? (describe-time 63000) '(17.5 hours))
    (check sent-equal? (describe-time 518400) '(6 days)))
   (define/test
    superlative-tests
    (check-equal? (superlative 'dumb 'exercise) '(dumbest exercise))
    (check-equal? (superlative 'smart 'student) '(smartest student)))
   all-names)

