#lang sicp

;--------------------------------------------------------------
;Section 3.1, Exercise 3.1
(define (make-accumulator sum)
  (lambda (argument)
    (set! sum (+ sum argument))
    sum))

;--------------------------------------------------------------
;Section 3.1, Exercise 3.2
(define (make-monitored f)
  (let ((callCount 0))
    (define (resetCounter)
      (set! callCount 0)
      callCount)
    (define (dispatch m)
      (cond ((eq? m 'reset-count)
             (resetCounter))
            ((eq? m 'how-many-calls?)
             callCount)
            (else (begin
                    (set! callCount (+ callCount 1))
                    (f m)))))
    dispatch))


;--------------------------------------------------------------
;Section 3.1, Exercise 3.3,3.4
(define (make-account balance password)
  (let ((authAttempts 0)
        (passList (list password)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (makeJoint newPassword)
      (set! passList (append passList (list newPassword)))
      (dispatch newPassword)
      )
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      (error "Proceed to jail, do not pass go"))
    (define (dispatch pass)
      (lambda (passwordSupplied m)
      (cond ((eq? passwordSupplied pass)
             (set! authAttempts 0)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   ((eq? m 'makeJoint) makeJoint)
                   (else (error "Unknown request: DISPATCH" m))))
            (else
             (set! authAttempts (+ authAttempts 1))
             (if (>= authAttempts 7) (call-the-cops)
                 (error "Incorrect password provided:" passwordSupplied))
             ))))
  (dispatch password)))


;--------------------------------------------------------------
;Section 3.1, Exercise 3.5

(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((randX (random-in-range (* x1 1.0) x2))
          (randY (random-in-range (* y1 1.0) y2)))
      (p randX randY)))
  (* (monte-carlo trials experiment) (* (- x2 x1) (- y2 y1) 1.0 )))

;Estimates Pi:
;(estimate-integral
   ;(lambda (x y) (< (+ (square x) (square y)) 1))
   ;-2 2 -2 2 1000000)



;--------------------------------------------------------------
;Section 3.1, Exercise 3.6
(define rand
  (let ((x 1))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset resetX)
      (set! x resetX))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Invalid method for rand:" m))))
    dispatch))

;--------------------------------------------------------------
;Section 3.1, Exercise 3.7
(define (make-joint account currentPass newPass)
  ((account currentPass 'makeJoint) newPass))




;--------------------------------------------------------------
;Utility functions for this section:

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds")))

(define (fetchItem list ID)
  (cond ((> ID (- (length list) 1))
         (error "List index out of range"))
        ((= ID 0) (car list))
        (else (fetchItem (cdr list) (- ID 1)))))












(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (square x) (* x x))

(define (rand-update x)
  (let ((a 1010102021)
        (b 123091919)
        (m 120))
    (remainder
     (+
      (* a x)
      b)
     m)))


(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n) product
        (iter (* product counter)
              (+ counter 1))))
  (iter 1 1))

(define (factorialImp n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin
            (set! product (* counter product))
            (set! counter (+ counter 1))
            (iter))))
    (iter)))

(define f
  (let ((product 1))
    (lambda (n)
      (set! product (* product n))
      product)))

(define (make-withdraw-2 initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance
                       (- balance amount))
                 (set! initial-amount (* initial-amount 2))
                 initial-amount)
          "Insufficient funds"))))

(define (make-account3 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance 
                        amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                        MAKE-ACCOUNT" 
                       m))))
    dispatch)