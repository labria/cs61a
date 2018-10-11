#lang racket

(require berkeley)
(provide (all-defined-out))

;;Lesson 10

;;Exercise 1
;;SICP 3.5.1
#| Explain



|#

;;Exercise 2
;;SICP 3.5.1
#| Explain


|#

;;Exercise 3
#| Explain


|#

;;Exercise 4
;a.
(define (num-seq n)
  (error "Not yet implemented"))

;b.
(define (seq-length stream)
  (error "Not yet implemented"))

;;Exercise 5
;;3.50
;;Fill in the <??> in the code given below. 
;;Then, uncomment the code and remove the (error "Not yet implemented")
;;NOTE: For this problem only, use my-stream-map instead of stream-map
(define (my-stream-map proc . argstreams)
  (error "Not yet implemented")
#|  (if (<??> (car argstreams))
      the-empty-stream
      (<??>
       (apply proc (map <??> argstreams))
       (apply my-stream-map
              (cons proc (map <??> argstreams)))))|#
              )

;;3.51
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
#| Returns:


|#
(stream-ref x 7)
#| Returns:


|#


;;3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

#| What is the value for 'sum'?


|#

#| What is the printed response to evaluating the stream-ref and display-stream?


|#

#| Will it be diffferent if we implemented (delay <exp>) as (lambda () <exp>)


|#
;;3.53
#|Describe the elements of the stream


|#

;;3.54
(define (mul-streams s1 s2)
  (error "Not yet implemented"))

;;3.55
(define (partiam-sums stream)
  (error "Not yet implemente"))

;;3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

#| Uncomment this after you defined it 
(define S (cons-stream 1 (merge <??> <??>)))
|#

;;3.64
(define (stream-limit stream tolerance)
  (error "Not yet implemented"))

;;3.66
#| Explain


|#

;;3.68
#| Explain



|#


;;Exercise 6
(define (fract-stream lst)
  (error "Not yet implemented"))
