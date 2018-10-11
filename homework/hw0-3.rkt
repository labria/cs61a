#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  (define (pl-secs num wd)
    (word wd 's))
  (cond ((or (not (integer? secs))
             (< secs 0))
         (error "Please enter a valid time format"))
        ((< secs 60)
         (se secs
             (pl-secs secs 'second)))
        ((< secs (* 60 60))
         (se (quotient secs 60)
             (pl-secs (quotient secs 60) 'minute)
             (describe-time (remainder secs 60))))
        ((< secs (* 60 60 24))
         (se (quotient secs (* 60 60))
             (pl-secs (quotient secs (* 60 60)) 'hour)
             (describe-time (remainder secs (* 60 60)))))
        (else
         (se (quotient secs (* 60 60 24))
             (pl-secs (quotient secs (* 60 60 24)) 'day)
             (describe-time (remainder secs (* 60 60 24)))))))

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (cond ((empty? sent) '())
        ((equal? wd (first sent))
         (se (bf sent)))
        (else
         (se (first sent)
             (remove-once wd (bf sent))))))

; Exercise 3 - Define differences
(define (differences nums)
  (if (= (count nums) 1)
      '()
      (se (- (second nums) (first nums))
          (differences (bf nums)))))

; Exercise 4 - Define location
(define (location small big)
  (define num 0)
  (define (loc-recur small big)
    (cond ((empty? big)
           #f)
          ((equal? small (first big))
           (+ num 1))
          (else
           (set! num (add1 num))
           (loc-recur small (bf big)))))
  (loc-recur small big))

; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent))
          (initials (bf sent)))))

; Exercise 6 - Define copies
(define (copies num wd)
  (if (<= num 0)
      '()
      (se wd (copies (- num 1) wd))))

; Exercise 7 - Define gpa
(define (base-grade grade)
  (cond ((equal? (first grade) 'A) 4.00)
        ((equal? (first grade) 'B) 3.00)
        ((equal? (first grade) 'C) 2.00)
        ((equal? (first grade) 'D) 1.00)
        ((equal? (first grade) 'F) 0.00)
        (else (error "Invalid grade format"))))
(define (grade-modifier grade)
  (define (second wd) (first (bf wd)))
  (cond ((empty? (bf grade)) 0)
        ((equal? (second grade) '+) 0.33)
        ((equal? (second grade) '-) -0.33)
        (else (error "Invalid grade format"))))
(define (comb-grade grade)
  (+ (base-grade grade)
     (grade-modifier grade)))
(define (gpa grades)
  (define (gpa-recurs grades)
    (if (= (count grades) 1)
        (comb-grade (first grades))
        (+ (comb-grade (first grades))
           (gpa-recurs (bf grades)))))
  (/ (gpa-recurs grades)
     (count grades)))

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  (cond ((empty? sent) '())
        ((number? (first sent))
         (se (copies (first sent) (second sent))
             (repeat-words (bf (bf sent)))))
        (else
         (se (first sent)
             (repeat-words (bf sent))))))

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  (define (count-ltrs sent)
    (if (empty? sent)
        '()
        (se (count (first sent))
            (count-ltrs (bf sent)))))
  (and (equal? (count sent1)
               (count sent2))
       (equal? (count-ltrs sent1)
               (count-ltrs sent2))))