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
  ; your code here
  (error "Not yet implemented")
)

;;2. Define two-first
(define (two-first x y)
  ; your code here
  (error "Not yet implemeted")
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 2 - Define teen?
(define (teen? num)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 3 - Define indef-article
(define (indef-article wd)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 5 - Define query
(define (query sent)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  ; your code here
  (error "Not yet implemented")
)

(define (american-time time)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  ; your code here
  (error "Not yet implemented")
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective word)
  (se (word adjective 'est) word))

#|

Explanation here.

|#