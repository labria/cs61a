; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
  ; Your code here
  (error "Not yet implemented")
)

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
Your explanation here

|#

; Exercise 4 - Define squares

(define (squares sent)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 5 - Define switch

(define (switch sent)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 6 - Define ordered?

(define (ordered? sent)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 7 - Define ends-e

(define (ends-e sent)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 8

#|
Your explanation here

|#