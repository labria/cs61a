#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom

;2. Compound Expression (3 Atoms)

;3. Compound Expression (4 Atoms)

;4. Compound Expression (1 Atom and 2 subexpressions)

;5. Any Other Kind Expression


;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  (word (first wd) (second wd)))

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y)))

;;3. Define two-first-sent
(define (two-first-sent sent)
  (two-first (first sent) (second sent)))

;Exercise 2 - Define teen?
(define (teen? num)
  (and (>= num 13)
       (<= num 19)))

;Exercise 3 - Define indef-article
(define (indef-article wd)
  (if (member? (first wd) 'aeiou)
      (se 'an wd)
      (se 'a wd)))

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (se (bl sent) 'and (last sent)))

;Exercise 5 - Define query
(define (query sent)
  (se (second sent)
      (first sent)
      (bf (bf (bl sent)))
      (word (last sent) '?)))

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (cond ((= (first time) 12)
         (cond ((equal? (second time) 'am) 0)
               ((equal? (second time) 'pm) 12)
               (else (error "Please enter a valid time format"))))
        ((and (integer? (first time))
              (>= (first time) 1)
              (<= (first time) 11))
         (if (equal? (second time) 'am)
             (first time)
             (+ (first time) 12)))
        (else (error "Please enter a valid time format"))))

(define (american-time time)
  (cond ((= time 0) '(12 am))
        ((= time 12) '(12 pm))
        ((and (integer? time)
              (>= time 1)
              (<= time 23))
         (if (> time 12)
             (se (- time 12) 'pm)
             (se time 'am)))
        (else (error "Please enter a valid time format"))))

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (cond ((or (not (integer? secs))
             (< secs 0))
         (error "Please enter a valid time format"))
        ((< secs 60)
         (se secs 'seconds))
        ((< secs (* 60 60))
         (se (/ secs 60.0) 'minutes))
        ((< secs (* 60 60 24))
         (se (/ secs (* 60.0 60)) 'hours))
        ((< secs (* 60 60 24 365.25))
         (se (/ secs (* 60.0 60 24)) 'days))
        (else (error "Please enter a valid time format"))))

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

The built-in procedure "word" has been replaced with whatever was passed as the second
argument to "superlative". Thus, the "word" built-in is no longer available within the
scope of the procedure. Change the "word" variable in the superlative procedure to fix

|#