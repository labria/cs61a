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
  ; (define/test
  ;  count-ums-tests
  ;  (check-equal?
  ;   (count-ums
  ;    '(today um we are going to um talk about the combining um method))
  ;   3)
  ;  (check-equal? (count-ums '(um um um um um)) 5)
  ;  (check-equal? (count-ums '(this is a um sentence um kind of)) 2))
  ; (define/test
  ;  countdown-tests
  ;  (check-equal? (countdown 10) '(10 9 8 7 6 5 4 3 2 1 blastoff!))
  ;  (check-equal? (countdown 3) '(3 2 1 blastoff!)))
  ; (define/test
  ;  numbers-tests
  ;  (check-equal? (numbers '(76 trombones and 110 cornets)) '(76 110))
  ;  (check-equal? (numbers '(but there are none)) '())
  ;  (check-equal?
  ;   (numbers '(5 golden rings 4 calling birds 3 french hens 2 turtle doves))
  ;   '(5 4 3 2)))
   (define/test
    describe-time-tests
    (define (sent-equal? x y)
      (define (item-equal? a b)
        (if (and (number? a) (number? b)) (= a b) (equal? a b)))
      (andmap item-equal? x y))
    (check sent-equal? (describe-time 40) '(40 seconds))
    (check sent-equal? (describe-time 22222) '(6 hours 10 minutes 22 seconds))
    (check sent-equal? (describe-time 550441) '(6 days 8 hours 54 minutes 1 seconds)))
   (define/test
    remove-once-tests
    (check member
           (remove-once 'morning '(good morning good morning))
           (list '(good good morning) '(good morning good)))
    (check member
           (remove-once 'the '(the spoon and the fork))
           (list '(the spoon and fork) '(spoon and the fork)))
    (check-equal? (remove-once 'fat '(a fat cat)) '(a cat))
    (check-equal? (remove-once 'world '(hello world)) '(hello)))
   (define/test
    differences-tests
    (check-equal? (differences '(4 23 9 87 6 12)) '(19 -14 78 -81 6))
    (check-equal? (differences '(8 12 9)) '(4 -3))
    (check-equal? (differences '(2 5 9)) '(3 4)))
   (define/test
    location-tests
    (check-equal? (location 'me '(you never give me your money)) 4)
    (check-false (location 'i '(you never give me your money)))
    (check-equal? (location 'the '(the fork and the spoon)) 1)
    (check-equal? (location 'o '(v i b g y o r)) 6))
   (define/test
    initials-tests
    (check-equal? (initials '(if i needed someone)) '(i i n s))
    (check-equal? (initials '(v i b g y o r)) '(v i b g y o r))
    (check-equal? (initials '(she loves you)) '(s l y)))
   (define/test
    copies-tests
    (check-equal? (copies 8 'spam) '(spam spam spam spam spam spam spam spam))
    (check-equal? (copies 1 'ham) '(ham))
    (check-equal? (copies 0 'nothing) '())
    (check-equal? (copies 2 'putt) '(putt putt)))
   (define/test
    gpa-tests
    (check-= (gpa '(A A+ B+ B)) 3.665 0.01)
    (check-= (gpa '(B)) 3 0.01)
    (check-= (gpa '(C- D+ C B-)) 1.9175 0.01)
    (check-= (gpa '(B- A+)) 3.5 0.01))
   (define/test
    repeat-words-tests
    (check-equal?
     (repeat-words '(4 calling birds 3 french hens))
     '(calling calling calling calling birds french french french hens))
    (check-equal?
     (repeat-words '(the 7 samurai))
     '(the samurai samurai samurai samurai samurai samurai samurai))
    (check-equal?
     (repeat-words '(first 1 ant then 2 ants))
     '(first ant then ants ants)))
   (define/test
    same-shape?-tests
    (check-true (same-shape? '(the fool on the hill) '(you like me too much)))
    (check-false
     (same-shape? '(the fool on the hill) '(and your bird can sing)))
    (check-false (same-shape? '(the fool on the hill) '(the fool on)))
    (check-false (same-shape? '(a pie) '(an egg)))
    (check-true (same-shape? '(a pie) '(a dvd))))
   all-names)

