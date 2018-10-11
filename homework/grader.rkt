#lang racket

(require racket/sandbox)

(provide main)

(define-namespace-anchor a)

(define (get-evaluator submission-file)
  (define e
    (make-module-evaluator
     `(module m racket
	      (require (planet dyoo/simply-scheme))
	      (require rackunit rackunit/text-ui)
	      (require (file ,submission-file)))
     #:allow-for-require (list '(planet dyoo/simply-scheme)
			       'rackunit
			       'rackunit/text-ui
			       submission-file)))

  (e '(define test-map (make-hash)))
  e)

(define (grade assignment-file submission-file questions)
  (define ns (namespace-anchor->empty-namespace a))
  (parameterize ([sandbox-eval-limits '(10 20)]
		 ;; TODO: This is horrendous, why does rackunit need to see if files exist
		 [sandbox-path-permissions (list '(exists "/"))]
		 [current-namespace ns])
    (define create-tests
      (dynamic-require (simplify-path (cleanse-path assignment-file))
		       'create-tests))

    (define e (get-evaluator submission-file))
    (define question-names
      (map (lambda (name) (string-append name "-tests"))
	   questions))

    (define all-names (create-tests e question-names))

    (when (null? question-names)
	  (set! question-names all-names))

    (for ([q question-names])
	 (unless (e `(hash-has-key? test-map ,q))
		 (display (format "Unknown question: ~a\n" q))
		 (raise 'exit)))

    (define num-questions (length question-names))

    (define num-failed
      (for/sum ([q question-names])
	 (display (format "Running tests: ~a\n" q))
	 (e `(run-tests (hash-ref test-map ,q) 'verbose))))

    (if (= num-failed 0)
	(display (format "All ~a tests passed!\n"
                         (length question-names)))
	(display (format "Failed ~a out of ~a tests.\n"
			 num-failed
			 (length question-names))))

    (/ (- num-questions num-failed) num-questions)))

(define (main assignment-file submission-file . args)
  (define (option? str)
    (equal? (string-ref str 0) #\-))
  (define-values (options questions)
    (partition option? args))

  (define (on-error x)
    (display
     (format "An error occurred:\n~a\nMake sure that ~a loads into Racket by typing 'racket ~a'.\n"
	     (exn-message x)
	     submission-file
	     submission-file)))

  (if (or (member "--quiet" options) (member "-q" options))
      (with-handlers ([(lambda (x) (equal? x 'exit))
		       (lambda (x) (display "-1\n"))]
		      [exn:fail?
		       (lambda (x) (display "-1\n"))])
	(define out (open-output-string))
	(define result
	  (parameterize ([current-output-port out]
			 [current-error-port out])
	    (exact->inexact
	     (grade assignment-file submission-file questions))))
	(close-output-port out)
	result)
      (with-handlers ([(lambda (x) (equal? x 'exit)) void]
		      [exn:fail? on-error])
        (exact->inexact
	 (grade assignment-file submission-file questions)))))
