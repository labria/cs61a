#lang racket

(provide add-question make-test make-error-test side-effect write-main)

(define code '())
(define (add-code x)
  (set! code (append code (list x))))

(display "#lang racket\n\n")
(add-code '(require racket/sandbox))
(add-code '(provide (all-defined-out)))

(define test-code '())
(define (add-test-code x)
  (set! test-code (append test-code (list x))))

;; Wrapper around pretty-print to remove the initial quote and to add
;; a newline at the end
(define (pp thing [newline? #t])
  (display
   (substring (with-output-to-string
		(lambda () (pretty-print thing)))
	      1))
  (when newline? (newline)))

(define (symbol-append x y)
  (string->symbol (string-append (symbol->string x)
				 (symbol->string y))))

(define (add-question name vars . tests)
  (define suite-name (symbol-append name '-tests))
  (define namestr (symbol->string name))
  (add-test-code
   `(define/test ,suite-name ,@tests)))

(define-syntax (make-test stx)
  (syntax-case stx (hidden quote equality member)
    [(make-test (quote code) #t)
     (syntax/loc stx
       '(check-true code))]
    [(make-test (quote code) #t hidden)
     (syntax/loc stx
       '(check-true code))]
    [(make-test (quote code) #f)
     (syntax/loc stx
       '(check-false code))]
    [(make-test (quote code) #f hidden)
     (syntax/loc stx
       '(check-false code))]
    [(make-test (quote code) result)
     (syntax/loc stx
       '(check-equal? code result))]
    [(make-test (quote code) result hidden)
     (syntax/loc stx
       '(check-equal? code result))]
    [(make-test (quote code) result (equality member) member)
     (syntax/loc stx
       '(check member code result))]
    [(make-test (quote code) result (equality eq))
     (syntax/loc stx
       '(begin
          (define (sent-equal? x y)
            (define (item-equal? a b)
              (if (and (number? a) (number? b))
                  (= a b)
                  (equal? a b)))
            (andmap item-equal? x y))
          (check sent-equal? code result)))]
    [(make-test (quote code) result (equality eq) hidden)
     (syntax/loc stx
       '(begin
          (define (sent-equal? x y)
            (define (item-equal? a b)
              (if (and (number? a) (number? b))
                  (= a b)
                  (equal? a b)))
            (andmap item-equal? x y))
          (check sent-equal? code result)))]))

(define-syntax (make-error-test stx)
  (syntax-case stx (hidden quote)
    [(make-error-test (quote code) a b)
     (syntax/loc stx
       '(check-exn exn:fail? (lambda () code)))]))

(define (side-effect code)
  code)

(define (write-main)
  (add-code
   `(define (create-tests e questions)
      (define all-names '())
	 
      (define-syntax-rule (define/test name code ...)
	(when (or (null? questions) (member (symbol->string 'name) questions))
	      (set! all-names (append all-names (list (symbol->string 'name))))
	      (e '(define name
		    (test-suite (symbol->string 'name)
				(test-case (symbol->string 'name)
					   code ...))))
	      (e '(hash-set! test-map (symbol->string 'name)
			     name))))
      ,@test-code
      all-names))

  (for-each pp code))
