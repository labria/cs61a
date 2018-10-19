#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  (cond ((empty? sent) '())
        ((member? (first sent) (bf sent))
         (dupls-removed (bf sent)))
        (else
         (se (first sent) (dupls-removed (bf sent))))))

; Exercise 2 - Define count-word

(define (count-word sent wd)
  (cond ((empty? sent) 0)
        ((equal? (first sent) wd)
         (+ 1 (count-word (bf sent) wd)))
        (else
         (count-word (bf sent) wd))))

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|

The recursive case will always be evaluated, since new-if is not a special form, skipping the base case
and resulting in an infinite loop.

|#

; Exercise 4 - Define squares

(define (squares sent)
  (if (empty? sent)
      '()
      (se (square (first sent))
          (squares (bf sent)))))

; Exercise 5 - Define switch

(define (switch sent)
  (define (I? wd)
    (equal? wd 'I))
  (define (me? wd)
    (equal? wd 'me))
  (define (you? wd)
    (equal? wd 'you))
  (define (_switch sent)
    (cond ((empty? sent) '())
          ((or (I? (first sent)) (me? (first sent)))
           (se 'you (_switch (bf sent))))
          ((you? (first sent))
           (se 'me (_switch (bf sent))))
          (else
           (se (first sent) (_switch (bf sent))))))
  (if (you? (first sent))
      (se 'I (_switch (bf sent)))
      (_switch sent)))

; Exercise 6 - Define ordered?

(define (ordered? sent)
  (define (second sent)
    (first (bf sent)))
  (define (done?)
    (= (count sent) 1))
  (define (not-ordered?)
    (> (first sent) (second sent)))
  (cond ((done?) #t)
        ((not-ordered?) #f)
        (else (ordered? (bf sent)))))

; Exercise 7 - Define ends-e

(define (ends-e sent)
  (define (e-wd? wd)
    (equal? (last wd) 'e))
  (cond ((empty? sent) '())
        ((e-wd? (first sent))
         (se (first sent)
             (ends-e (bf sent))))
        (else
         (ends-e (bf sent)))))

; Exercise 8

#|

(define (p) (p))

OR:

(or #t (p))  If or was an ordinary procedure, the infinite recursion will be triggered.
If special form, it will stop and return #t.

AND:

(and #f (p)) Same as for 'or, but it will return #f if special form.

|#
