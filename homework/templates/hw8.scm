; SICP 3.3, 3.4 - Modify make-password-account to make it generate
; password-protected accounts.
; Also, if an incorrect password is given 7 times consecutively, you
; should say (call-the-cops).
; Note: In the case of a wrong password, you should return the string
; "Incorrect password".  Do not use display or print or error.

(define (make-password-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; SICP 3.7 - Define make-joint.
; You may want to modify make-password-account, but you shouldn't
; remove any of its previous functionality.


; SICP 3.8 - Define reset-f!
; This is a function that defines the function f that the exercise
; asks for.

(define f #f)

(define (reset-f!)
  (set! f ??))

; For example, if you think that f should be the square function, you
; would say:
; (define (reset-f!)
;   (set! f (lambda (x) (* x x))))

; SICP 3.10 - Answer in the comment block.
#|
You have two options:
1) Draw the environment diagram here using ASCII art
2) Submit a .jpg or .pdf file containing your environment diagram (for
example, you could scan a paper drawing), in which case you should
write the filename here.

Environment diagram here

Q. How do the environment structures differ for the two versions?
A. 

Q. Show that the two versions of make-withdraw create objects with the
same behavior.
A. 

|#

; SICP 3.11 - Answer in the comment block.
#|
Same options as in 3.10

Environment diagram here

Q. Where is the local state for acc kept?
A.

Q. How are the local states for the two accounts kept distinct?
A.

Q. Which parts of the environment structure are shared?
A. 

|#
