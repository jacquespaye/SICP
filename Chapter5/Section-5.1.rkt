#lang sicp

(#%require "Section-5.2.rkt")

;--------------------------------------------------------------
;Section 5.1, Exercise 5.2
(define fact-machine
  (make-machine
   (list (list '+ +) (list '> >) (list '* *) (list 'print display) (list 'read read))
   '(init
     (assign product (const 1))
     (assign counter (const 1))
     (assign n (op read))
     test-greater
     (test (op >) (reg counter) (reg n))
     (branch (label fact-done))
     (assign product (op *) (reg product) (reg counter))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-greater))
     fact-done
     (perform (op print) (reg product))
     (perform (op print) (const "\n"))
     (goto (label init)))))


;--------------------------------------------------------------
;Section 5.1, Exercise 5.3
(define root-machine
  (make-machine
   (list
    (list 'print display)
    (list 'read read)
    (list '* *)
    (list '- -)
    (list '< <)
    (list '/ /)
    (list '+ +))
   '(init
     (assign x (op read))
     (assign guess (const 1.0))
     
     test-guess
     (assign t (op *) (reg guess) (reg guess))
     (assign t (op -) (reg t) (reg x))
     abs-t
     (test (op <) (reg t) (const 0))
     (branch (label change-sign))
     (goto (label abs-done))
     change-sign
     (assign t (op *) (reg t) (const -1))
     abs-done
     
     (test (op <) (reg t) (const 0.001))
     (branch (label root-done))
     (assign t (op /) (reg x) (reg guess))
     (assign t (op +) (reg t) (reg guess))
     (assign guess (op /) (reg t) (const 2))
     (goto (label test-guess))
     
     root-done
     (perform (op print) (reg guess))
     (perform (op print) (const "\n"))
     (goto (label init)))))

;--------------------------------------------------------------
;Section 5.1, Exercise 5.3
(define expt-machine
  (make-machine
   (list
    (list 'print display)
    (list 'read read)
    (list '- -)
    (list '* *)
    (list '= =))
   '(init
     (assign b (op read))
     (assign n (op read))
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done
     (perform (op print) (reg val))
     (perform (op print) (const "\n"))
     (goto (label init)))))

(define expt-machine-iter
  (make-machine
   (list
    (list 'print display)
    (list 'read read)
    (list '- -)
    (list '* *)
    (list '= =))
   '(init
     (assign b (op read))
     (assign n (op read))
     (assign counter (reg n))
     (assign product (const 1))
     expt-iter
     (test (op =) (reg counter) (const 0))
     (branch (label expt-done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-iter))
     expt-done
     (perform (op print) (reg product))
     (perform (op print) (const "\n"))
     (goto (label init)))))

;--------------------------------------------------------------
;Other register machine designs below

(define gcd-machine-og
  (make-machine
   (list (list 'rem remainder) (list '= =) (list 'read read))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(define gcd-machine
  (make-machine
   (list (list '< <) (list '- -) (list '= =) (list 'print display) (list 'read read))
   '(init
     (assign a (op read))
     (assign b (op read))
     test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (reg a))
     rem-loop
       (test (op <) (reg t) (reg b))
       (branch (label rem-done))
       (assign t (op -) (reg t) (reg b))
       (goto (label rem-loop))
     rem-done
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done
     (perform (op print) (reg a))
     (perform (op print) (const "\n"))
     (goto (label init)))))





(define (square x) (* x x))
(define (good-enough? guess x)
  (display "\nguess:") (display guess)
  (display "\nx:") (display x)
  (< (abs (- (square guess) x)) 0.001))
(define (average a b)
  (/ (+ a b) 2.0))
(define (improve guess x)
  (average guess (/ x guess)))






(define fivePointEight
  (make-machine
   (list (list 'print display))
   '(start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     heere
     (assign a (const 4))
     (goto (label there))
     there
     (perform (op print) (reg a))
     )))



    
(define fivePointNine
  (make-machine
   (list (list 'print display)
         (list '+ +))
   '(start
     (goto (label here))
     here
     (assign a (op +) (const 3) (const 3))
     (goto (label there))
     there
     (perform (op print) (reg a))
     )))

(define fib-machine
  (make-machine
   (list (list 'print display)
         (list 'read read)
         (list '+ +)
         (list '* *)
         (list '- -)
         (list '< <))
   '(init
     (assign n (op read))
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n − 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)           ; save old value of n
     (assign n 
             (op -)
             (reg n)
             (const 1)) ; clobber n to n-1
     (goto 
      (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n − 1)
     (restore n)
     ;; set up to compute Fib(n − 2)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)         ; save Fib(n − 1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n − 2)
     ;(restore n) ; we transfer the old value of n (fib(n-1)) into the n register so we can add it to the current val (which contains fib(n-2))
     (assign n 
             (reg val)) ; n now contains Fib(n − 2)
     (restore val)
     (restore continue)
     (assign val        ; Fib(n − 1) + Fib(n − 2)
             (op +) 
             (reg val)
             (reg n))
     (goto              ; return to caller,
      (reg continue))   ; answer is in val
     immediate-answer
     (assign val 
             (reg n))   ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done
     (perform (op print) (reg val))
     (perform (op print) (const "\n"))
     (goto (label init)))))


    
(define fivePointEleven
  (make-machine
   (list (list 'print display)
         (list '+ +))
   '(start
     (assign a (const 1))
     (assign b (const 2))
     (assign c (const 3))
     (assign d (const 4))
     (save d)
     (save a)
     (save b)
     (save c)
     (save d)
     (assign a (const 111))
     (save d)
     (restore a)


     
     (perform (op print) (reg a))
     (perform (op print) (reg b))
     (perform (op print) (reg c))
     (perform (op print) (reg d))
     )))


(define fib
  (make-machine
    `((= ,=))
    '(
      (save n)
      (assign n (const 40))
      (save n)

      (save val)
      (assign val (const 10))
      (save val)

      (restore n)
      (assign n1 (reg n))
      (restore n)

      (assign val (const 1))
      (restore val)

      )))

(define fact-machine-rec
  (make-machine
   (list
     (list 'read read)
     (list '= =)
     (list '- -)
     (list '* *)
     (list 'print display))
     
   '(init
     (perform (op initialize-stack))
     (assign n (op read))
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done
     (perform (op print) (reg val))
     (perform (op print) (const "\n"))
     (perform (op print-stack-statistics))
     (goto (label init)))))

(define fib3
  (make-machine
    `((= ,=))
    '(
      (save n)
      (assign n (const 40))
      (save n)
    george
      (save val)
      (assign val (const 10))
    just-a-label
      (save val)
      )))


(define fib4
  (make-machine
    `((= ,=)
      (+,+))
    '(
      (assign n (const 40))
    george
      (assign n (const 125))
    harry
      (assign n (op +) (reg n) (const 9))
      )))






(define cons-machine
  (make-machine
   `((read,read)
     (print,display)
     (make-vector, make-vector)
     (vector-ref,vector-ref)
     (vector-set!,vector-set!)
     (+,+)
     (eq?,eq?))
   '(init
     (assign free (const 0))
     (assign the-cars (op make-vector) (const 10))
     (assign the-cdrs (op make-vector) (const 10))
     
     cons-loop
     (perform (op print) (const "Car-"))
     (assign cons-car (op read))
     (test (op eq?) (reg cons-car) (const next))
     (branch (label lookup))
     (perform (op print) (const "Cdr-"))
     (assign cons-cdr (op read))
     
     ;(put (cons a b) in free)
     (perform (op vector-set!) (reg the-cars) (reg free) (reg cons-car))
     (perform (op vector-set!) (reg the-cdrs) (reg free) (reg cons-cdr))
     (assign free (op +) (reg free) (const 1))
     (goto (label cons-loop))

     lookup
     (perform (op print) (const "\n Which register? "))
     ;fetch car or cons of a given pair
     (assign desired-reg (op read))
     (perform (op print) (const "\n Car or cdr? "))
     (assign car-or-cdr (op read))
     (test (op eq?) (reg car-or-cdr) (const car))
     (branch (label lookup-car))
     (test (op eq?) (reg car-or-cdr) (const cdr))
     (branch (label lookup-cdr))
     (goto (label error))

     lookup-car
     (assign output (op vector-ref) (reg the-cars) (reg desired-reg))
     (perform (op print) (reg output))
     (goto (label lookup))

     lookup-cdr
     (assign output (op vector-ref) (reg the-cdrs) (reg desired-reg))
     (perform (op print) (reg output))
     (goto (label lookup))

     error
     (perform (op print) (const "Input must be car or cdr, looping")))))
     




