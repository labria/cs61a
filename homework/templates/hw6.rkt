#lang racket

(require berkeley)
(provide (all-defined-out))

;;for data directed programming
;;YOU CANNOT CLEAR THE GET-PUT TABLE. IF YOU WOULD LIKE TO DO, YOU MUST RESTART RACKET AND RELOAD YOUR FILES
(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

;;for tagged-data programming
(define (attach-tag tag data)
    (cons tag data))
(define (type-tag tagged-data)
    (if (pair? tagged-data)
        (car tagged-data)
        (error "Not tagged data")))
(define (contents tagged-data)
    (if (pair? tagged-data)
        (cdr tagged-data)
        (error "Not tagged data")))


;; Lesson 6

;; Exercise 1

#|
2.74  Note there is no need to write code here - this is a thought exercise.
a. Explain.

b. Explain.

c. Explain.

d. Explain.

|#

;; 2.75 Define make-from-mag-ang-mp.
;(define (square x) (* x x))

(define (make-from-real-imag-mp x y)
  (define (dispatch op)
    (cond ((eq? op 'real-val) x)
          ((eq? op 'imag-val) y)
          ((eq? op 'mag)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'ang) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic-message-passing op arg) (arg op))

(define (real-val-mp z) (apply-generic-message-passing 'real-val z))
(define (imag-val-mp z) (apply-generic-message-passing 'imag-val z))
(define (mag-mp z) (apply-generic-message-passing 'mag z))
(define (ang-mp z) (apply-generic-message-passing 'ang z))

(define (make-from-mag-ang-mp r a)
  '(YOUR CODE HERE)
)

#|
2.76 Describe the changes.

|#

#|
2.77 Explain.

|#

;; The packages for dealing with different types of numbers.

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-racket-number-package)
  (define (tag x)
    (attach-tag 'racket-number x))
  (put 'add '(racket-number racket-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(racket-number racket-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(racket-number racket-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(racket-number racket-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'racket-number
       (lambda (x) (tag x)))
  'done)

(define (make-racket-number n)
  ((get 'make 'racket-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-val z) (car z))
  (define (imag-val z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (mag z)
    (sqrt (+ (square (real-val z))
             (square (imag-val z)))))
  (define (ang z)
    (atan (imag-val z) (real-val z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-val '(rectangular) real-val)
  (put 'imag-val '(rectangular) imag-val)
  (put 'mag '(rectangular) mag)
  (put 'ang '(rectangular) ang)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (mag z) (car z))
  (define (ang z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-val z)
    (* (mag z) (cos (ang z))))
  (define (imag-val z)
    (* (mag z) (sin (ang z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-val '(polar) real-val)
  (put 'imag-val '(polar) imag-val)
  (put 'mag '(polar) mag)
  (put 'ang '(polar) ang)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-val z) (apply-generic 'real-val z))
(define (imag-val z) (apply-generic 'imag-val z))
(define (mag z) (apply-generic 'mag z))
(define (ang z) (apply-generic 'ang z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-val z1) (real-val z2))
                         (+ (imag-val z1) (imag-val z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-val z1) (real-val z2))
                         (- (imag-val z1) (imag-val z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (mag z1) (mag z2))
                       (+ (ang z1) (ang z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (mag z1) (mag z2))
                       (- (ang z1) (ang z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define put-coercion put)
(define get-coercion get)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; 2.79 Define equ? for racket-number, rational, and complex packages.
;; You will need to use put to install equ? in each of the packages.

(define (equ? a b)
  (apply-generic 'equ? a b))

;; 2.80 Define =zero? for racket-number, rational, and complex packages.

(define (=zero? num)
  (apply-generic '=zero? num))

#|
2.81
a. Explain.

b. Explain.

|#

;; c. Modify apply-generic.
;; You may want to change the name to "apply-generic" for testing.
;; However, on submitting it should be apply-generic-better-coercion.

(define (apply-generic-better-coercion op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags)))))) 
              (error "No method for these types"
                     (list op type-tags)))))))

;; 2.83 Install a raise-num procedure for every type except complex
;; Types: Integer -> Rational -> Real (racket number) -> Complex

(define (make-integer n)
  (attach-tag 'integer n))

(define (raise-num num)
  (apply-generic 'raise-num num))


;; Here is Racket-1, taken from ~cs61as/lib/racket1.rkt

(define (racket-1)
  (newline)
  (display "Racket-1: ")
  (flush-output)
  (print (eval-1 (read)))
  (newline)
  (racket-1)
  )

(define (eval-1 exp)
  (cond ((constant? exp) exp)
        ((symbol? exp) (eval exp))  ; use underlying Racket's EVAL
        ((quote-exp? exp) (cadr exp))
        ((if-exp? exp)
         (if (eval-1 (cadr exp))
             (eval-1 (caddr exp))
             (eval-1 (cadddr exp))))
        ((lambda-exp? exp) exp)
        ((pair? exp) (apply-1 (eval-1 (car exp))      ; eval the operator
                              (map eval-1 (cdr exp))))
        (else (error "bad expr: " exp))))

(define (apply-1 proc args)
  (cond ((procedure? proc)  ; use underlying Racket's APPLY
         (apply proc args))
        ((lambda-exp? proc)
         (eval-1 (substitute (caddr proc)   ; the body
                             (cadr proc)    ; the formal parameters
                             args           ; the actual arguments
                             '())))         ; bound-vars, see below
        (else (error "bad proc: " proc))))


(define (constant? exp)
  (or (number? exp) (boolean? exp) (string? exp) (procedure? exp)))

(define (exp-checker type)
  (lambda (exp) (and (pair? exp) (eq? (car exp) type))))

(define quote-exp? (exp-checker 'quote))
(define if-exp? (exp-checker 'if))
(define lambda-exp? (exp-checker 'lambda))

(define (substitute exp params args bound)
  (cond ((constant? exp) exp)
        ((symbol? exp)
         (if (memq exp bound)
             exp
             (lookup exp params args)))
        ((quote-exp? exp) exp)
        ((lambda-exp? exp)
         (list 'lambda
               (cadr exp)
               (substitute (caddr exp) params args (append bound (cadr exp)))))
        (else (map (lambda (subexp) (substitute subexp params args bound))
                   exp))))

(define (lookup name params args)
  (cond ((null? params) name)
        ((eq? name (car params)) (maybe-quote (car args)))
        (else (lookup name (cdr params) (cdr args)))))

(define (maybe-quote value)
  (cond ((lambda-exp? value) value)
        ((constant? value) value)
        ((procedure? value) value)  ; real Racket primitive procedure
        (else (list 'quote value))))

;; Exercise 2 - Add map-1 to Racket-1.
;; Make sure you understand why plain old map does not work (see lab exercise).

;; Exercise 3 - Add let to Racket-1.


;; Ordered list representation of sets
(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-ordered-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-ordered-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-ordered-set set1 (cdr set2)))))))

;; Exercise 4 - 2.62 Define union-ordered-set for the ordered list representation given above


;; Binary search tree representation of sets

;; The Binary Tree ADT
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-binary-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-binary-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-binary-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-binary-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; Exercise 5
;;Uncomment the following definitions and replace the zeros below with 
;;appropriate numbers to construct the trees

#|
(define tree1  ; Left tree
  (adjoin-set 0
    (adjoin-set 0
      (adjoin-set 0
        (adjoin-set 0
          (adjoin-set 0
            (adjoin-set 0 '())))))))

(define tree2  ; Middle tree
  (adjoin-set 0
    (adjoin-set 0
      (adjoin-set 0
        (adjoin-set 0
          (adjoin-set 0
            (adjoin-set 0 '())))))))

(define tree3  ; Right tree
  (adjoin-set 0
    (adjoin-set 0
      (adjoin-set 0
        (adjoin-set 0
          (adjoin-set 0
            (adjoin-set 0 '())))))))
|#

(install-racket-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
