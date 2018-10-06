#lang racket
(require berkeley)

;; Exercise 5.13

#|

(quote (quote banana))

The first element of ''banana is the word 'quote

|#

;; Exercise 5.14

(define (third wd)
  (item 3 wd))

;; Exercise 5.15

(define (first-two wd)
  (word (first wd) (first (bf wd))))

;; Exercise 5.16

(define (two-first a b)
  (word (first a) (first b)))

(define (two-first-sent sent)
  (define (helper sent)
    (word (first (first sent)) (first (bf sent))))
  (first-two (helper sent)))

;; Exercise 5.17

(define (knight sent)
  (se 'Sir sent))

;; Exercise 5.18

#|

(define (ends word)
  (word (first word) (last word)))

'word, here, is the argument to the 'ends procedure. As a result, word is bound as a local variable to
whatever argument is passed to ends, instead of the 'ends built-in procedure.

This should result in an error that the argument passed to 'ends is not defined as a procedure...
(unless it is)

|#

;; Exercise 5.19

(define (insert-and sent)
  (if (empty? (bf sent))
      (se 'and sent)
      (se (first sent) (insert-and (bf sent)))))

;; Exercise 5.20

(define (middle-names sent)
  (bf (bl sent)))

;; Exercise 5.21

(define (query sent)
  (se (item 2 sent) (first sent) (bf (bf (bl sent))) (word (last sent) '?)))