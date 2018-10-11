;;;;METACIRCULAR EVALUATOR THAT SEPARATES ANALYSIS FROM EXECUTION
;;;; FROM SECTION 4.1.7 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator of
;;;;  sections 4.1.1-4.1.4, since it uses the expression representation,
;;;;  environment representation, etc.
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.

;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.1.7 definition
;; of eval overrides the definition from 4.1.1
;; modified 8/2/2000 by jeremy to add path 

(load "~cs61as/lib/mceval.scm")


;;; Exercise 4.22
;;; In the ANALYZE procedure below, extend the evaluator in this section to support the special form let

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;; Exercise 4.23
#|
(define (analyze-sequence exps)
    (define (execute-sequence procs env)
          (cond ((null? (cdr procs)) ((car procs) env))
		          (else ((car procs) env)
				                (execute-sequence (cdr procs) env))))
      (let ((procs (map analyze exps)))
	    (if (null? procs)
		        (error "Empty sequence -- ANALYZE"))
	        (lambda (env) (execute-sequence procs env))))
|#


;;; Compare the two versions of analyze-sequence. For example, consider the common case (typical of procedure bodies) where the sequence has just one expression.
;;; What work will the execution procedure produced by Alyssa's program do?

#| Your answer Here:



|#


;;; What about the execution procedure produced by the program in the text above? How do the two versions compare for a sequence with two expressions?
#| Your answer here:



|#


;;; Exercise 4.27
;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-27a "your answer here")

;;; L-Eval input:
;;; w
;;; L-Eval value:
(define answer4-27b "your answer here")

;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-27c "your answer here")



;;; Exercise 4.29

;;; MEMOIZED VERSION:

;;; L-Eval input:
;;; (square (id 10))
;;; L-Eval value:
(define answer4-29a "your answer for memoized here")

;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-29b "your answer for memoized here")

;;; UNEMEMOIZED VERSION

;;; L-Eval input:
;;; (square (id 10))
;;; L-Eval value:
(define answer4-29c "your answer for unmemoized here")

;;; L-Eval input:
;;; count
;;; L-Eval value:
(define answer4-29d "your answer for unmemoized here")




;;; Exercise 4.25
#|
Suppose that (in ordinary applicative-order Scheme) we define unless as shown above and then define factorial in terms of unless as

(define (factorial n)
    (unless (= n 1)
	              (* n (factorial (- n 1)))
		                1))

What happens if we attempt to evaluate (factorial 5)? Will our definitions work in a normal-order language?





|#



;;; Exercise 4.26

#| Your answer here:



|#





;;; Execise 4.28
;;; Eval uses actual-value rather than eval to evaluate the operator before passing it to apply, in order to force the value of the operator.
;;; Give an example that demonstrates the need for this forcing.


#| Your example here




|#



;;; Exercise 4.30
;;; Part a
#| Your answer here



|#
;;; Part b

(define p1-original "your answer here")
(define p2-original "your answer here")

(define p1-changed "your answer here")
(define p2-changed "your answer here")

;;; Part c
#| Your answer here



|#


;;; Part d
#| Your answer here



|#



;;; Exercise 4.32
#| Your answer here



|#


;;; Exercise 4.33
;;; You can find the code for LAZY Evaluator below the code for analyzing evaluator
;;;
;;; To avoid overwriting analyzing's lazy-eval with lazy's, we have the procedures SETUP-ANALYZE and SETUP-LAZY that should be called before  calling (mce)
;;; If you want to make a change to Lazy's mc-eval, please change setup-lazy instead

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the ANALYZING evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mc-eval exp env)
  ((analyze exp) env))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;; Added at Berkeley:

(define input-prompt ";;; A-Eval input:")
(define output-prompt ";;; A-Eval value:")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Below is the LAZY evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To avoid overwriting analyzing's lazy-eval with lazy's, we have SETUP-ANALYZE and SETUP-LAZY that should be called before  (mce)
(define (setup-analyze)
  (set! input-prompt ";;; A-Eval input:")
  (set! output-prompt ";;; A-Eval value:")  
  (set! mc-eval (lambda (exp env)
	((analyze exp) env))))

(define (setup-lazy)
  (set! input-prompt ";;; L-Eval input:")
  (set! output-prompt ";;; L-Eval value:")
  (set! mc-eval (lambda (exp env)
		  (cond ((self-evaluating? exp) exp)
			((variable? exp) (lookup-variable-value exp env))
			((quoted? exp) (text-of-quotation exp) )
			((assignment? exp) (eval-assignment exp env))
			((definition? exp) (eval-definition exp env))
			((if? exp) (eval-if exp env))
			((lambda? exp)
			 (make-procedure (lambda-parameters exp)
					 (lambda-body exp)
					 env))
			((begin? exp) 
			 (eval-sequence (begin-actions exp) env))
			((cond? exp) (mc-eval (cond->if exp) env))
			((application? exp)             ; clause from book
			 (mc-apply (actual-value (operator exp) env)
				   (operands exp)
				   env))
			(else
			 (error "Unknown expression type -- EVAL" exp))))))

(define (actual-value exp env)
  (force-it (mc-eval exp env)))

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))



;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
  ;;; Used lazy list implementation (list is not defined this way yet)
  (list (list 'car (lambda (p) (p (lambda ( x y) x))))
        (list 'cdr (lambda (p) (p (lambda (x y) y))))
        (list 'cons (lambda (x y) (lambda (m) (m x y))))
	(list 'null? null?)
        (list 'list list)
	(list 'number? number?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

'LAZY-EVALUATOR-LOADED
