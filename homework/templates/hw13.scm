
;;; For each query and rule, please define them below as a list.
;; e.g.
#|
(define some-query
  '(and (supervisor ?x (Bitdiddle Ben))
	(not (job ?x (computer programmer))))
  )

(define some-rule
  '(rule (lives-near ?person-1 ?person-2)
	 (and (address ?person-1 (?town . ?rest-1))
	      (address ?person-2 (?town . ?rest-2))
	      (not (same ?person-1 ?person-2))))
  )

|#

;;; IMPORTANT: these variables are used for autograding only. You cannot use them 
;;; inside the query-driver-loop
 
(load "~cs61as/lib/query.scm")
;;; Exercise 4.56
;;; a
(define supervised-by-ben
  '()
  )

;;; b
(define salary-less-than-ben
  '()
  )

;;; c
(define supervised-not-in-cs
  '()
  )



;;; 4.57
(define replace
  '(rule (replace ?x ?y)
	 )
  )

(define replace-cydfect
  '()
  )

(define replace-paid-more
  '()
  )


;;; 4.58
(define bigshot
  '(rule (bigshot ?person ?division)
	 )
  )


;;; 4.65

#| Your answer here


|#
