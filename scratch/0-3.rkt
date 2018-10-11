#lang racket
(require berkeley)

(define (count-ums sent)
  (cond ((empty? sent)
         0)
        ((equal? (first sent) 'um)
         (+ 1 (count-ums (bf sent))))
        (else
         (count-ums (bf sent)))))

(define (countdown num)
  (if (= num 0)
      'blastoff!
      (se num (countdown (- num 1)))))

(define (vowel? ltr)
  (member? ltr 'aeiou))

(define (pigl wd)
  (if (vowel? (first wd))
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

;; Exercise 5

(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent))
          (initials (bf sent)))))

(define (numbers sent)
  (cond ((empty? sent) '())
        ((number? (first sent))
         (se (first sent)
             (numbers (bf sent))))
        (else
         (numbers (bf sent)))))