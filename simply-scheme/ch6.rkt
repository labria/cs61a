#lang racket
(require berkeley)

;; Exercise 6.5

(define (european-time hour)
  (cond ((and (integer? (first hour))
              (<= (first hour) 12)
              (or (equal? (bf hour) '(am))
                  (equal? (bf hour) '(AM)))) (first hour))
        ((and (integer? (first hour))
              (<= (first hour) 12)
              (or (equal? (bf hour) '(pm))
                  (equal? (bf hour) '(PM)))) (+ (first hour) 12))
        (else '(Please enter a valid time format))))

(define (american-time hour)
  (if (and (integer? hour)
           (>= hour 0)
           (<= hour 24))
      (cond ((= hour 12) '(12 pm))
            ((= hour 0) '(12 am))
            ((> hour 12) (se (- hour 12) 'pm))
            (else (se hour 'am)))
      '(Please enter a valid time format)))

;; Exercise 6.6

(define (teen? num)
  (and (>= num 13)
       (<= num 19)))

;; Exercise 6.7

(define (type-of arg)
  (cond ((number? arg) 'number)
        ((word? arg) 'word)
        ((sentence? arg) 'sentence)
        ((boolean? arg) 'boolean)
        (else '(This is something else.))))

;; Exercise 6.8

(define (vowel? ltr)
  (member? ltr 'aeiou))

(define (indef-article wd)
  (if (word? wd)
      (if (vowel? (first wd))
          (se 'an wd)
          (se 'a wd))
      '(Not a word)))