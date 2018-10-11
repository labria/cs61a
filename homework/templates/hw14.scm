(load "~cs61as/lib/concurrency.scm")
(load "~cs61as/lib/mapreduce/streammapreduce.scm")
(load "~cs61as/lib/query.scm")

;; NOTE: If you think that the possible final values are 10, 20 and 35, define the answer as '(10 20 35)

;;; Exercise 3.38
(define answer3.38a "your answer here")

#| Your answer here for part b


|#

;;; Exercise 3.39
(define answer3.39 "your answer here")

;;; Exercise 3.40
(define answer3.40a "your answer here")
(define answer3.40b "your answer here") ; Serialized


;;; Exercise 3.41

#| Your answer here



|#


;;; Exercise 3.42

#| Your answer here



|#



;;; Exercise 3.44

#| Your answer here



|#


;;; Exercise 3.46

#| Your answer here (or attach a diagram with your submission)



|#



;;; Exercise 3.48.  Explain in detail why the deadlock-avoidance method described above,
;;; (i.e., the accounts are numbered, and each process attempts to acquire the smaller-numbered account first)
;;; avoids deadlock in the exchange problem. Rewrite serialized-exchange to incorporate this idea.
;;; (You will also need to modify make-account so that each account is created with a number, which can be accessed by sending an appropriate message.)
#| Your answer here



|#


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))


;;; All accounts should have a number associated with it
;;; It should work with the ACCOUNT-NUMBER function defined below

(define id-generator
  (let ((mutex (make-mutex))
	(id 0))
    (lambda ()
      (mutex 'acquire)
      (set! id (+ id 1))
      (mutex 'release)
      id)))
  

(define (make-account-and-serializer balance)
  (define account-number (id-generator))
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'balance) balance)
	    ((eq? m 'serializer) balance-serializer)
	    ((eq? m 'account-number) account-number)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
	(d (account 'deposit)))
    ((s d) amount)))

(define (account-number account)
  (account 'account-number))



;;; Chaining Mapreduce
;;; Define REAL-MOST-FREQUENT with your mapper(s) and reducer(s)
;;; You should use two calls to MAPREDUCE
(define (real-most-frequent all-songs)
  (error "Not implemented yet!"))

;;; Do You Want to be the Very Best?
;;; Define your own MAPPER, REDUCER and BASECASE
;;; DATA is a variable that evaluates to stream of pokemon information
;;; (check the course site for details or just type "data" after loading this file in a scheme interpreter)
(define (pokemon-types)
  (mapreduce mapper reducer base-case data))
