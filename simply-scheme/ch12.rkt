#lang racket
(require berkeley)

(define (reverse wd)
  (if (= (count wd) 1)
      wd
      (word (last wd)
            (reverse (bl wd)))))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (evens sent)
  (define (second sent)
    (first (bf sent)))
  (if (<= (count sent) 1)
      '()
      (se (second sent)
          (evens (bf (bf sent))))))