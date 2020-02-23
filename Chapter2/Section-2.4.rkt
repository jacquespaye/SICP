#lang sicp


;--------------------------------------------------------------
;Section 2.4, Exercise 2.73

;data directed derivative procedure
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (sameVariable? exp var)
             1
             0))
        (else (get 'deriv (operator exp))
              (operands exp)
              var)))

;sum
(define (sumDeriv operands var)
  (makeSum (deriv (addend operands) var)
           (deriv (augend operands) var)))
;product
(define (productDeriv operands var)
  (makeSum
   (makeProduct (multiplier operands)
                (deriv (multiplicand operands) var))
   (makeProduct (multiplicand operands)
                (deriv (multiplier operands) var))))

(define (base exp)
  (car exp))
(define (exponent exp)
  (cadr exp))

;general exponent structure: (^ base exponent)
;exponent
(define (expDeriv operands var)
  (makeProduct
   (makeProduct (exponent operands)
                (makeExponent (base operands)
                              (makeSum (exponent operands) -1)))
   (deriv (base operands) var)))

(define (installDeriv)
  (put 'deriv '+ sumDeriv)
  (put 'deriv '* productDeriv)
  (put 'deriv '^ expDeriv)
  'done)


;--------------------------------------------------------------
;Section 2.4, Exercise 2.73--Insatiable Enterprises case study

;the individual divisions must agree on a common tagging structure
;for example, each file could be structured as a pair:
;(cons tag file)

;the divisions should then put a procedure to retrieve a specific record from a file
;into the common table. It should take two arguments: contents of the file, and the employee name
;(put 'getRecord 'Anaheim getRecordAnaheim) for example
;Should return false if it can't find the desired record

(define (fileType file)
  (car file))

(define (fileContents file)
  (cdr file))

;alternatively we could tag the records up here so that none of the divisions have to tag their records
(define (getRecord file employee)
   (let record ((get 'getRecord (fileType file)) (fileContents file) employee)
     (if record
         (tagRecord record (getTagFromFile file)))))

;The divisions should tag all their records with a unique identifier by division
;They should then provide a 'getSalary procedure, and put it into the common table associated with that record type
;(put 'getSalary 'Anaheim getSalaryAnaheim) for example

;each record could be structured as a cons pair: (cons tag record)

(define (recordType record)
  (car record))

(define (recordContents record)
  (cdr record))

(define (getSalary record)
  ((get 'getSalary (recordType record)) (recordContents record)))


(define (findEmployeeRecord fileList employee)
  (let ((record (getRecord (car fileList) employee)))
    (cond ((null? fileList) (error "findEmployeeRecord--no such employee:" employee)
          (record record)
          (else (findEmployeeRecord (cdr fileList) employee)))))

;If Insatiable takes over a new company, they won't have to make too many changes.
;The new company will need to tag its records and files according to the standard system
;that the other divisions use.
;The new company will then have to provide these types, as well
;as the relevant procedures for accessing records within files and data items within records.
;Once these are placed into Insatiable's table of procedures, the data directed design means that
;the new company's data should be integrated seamlessly into the system.

;--------------------------------------------------------------
;Section 2.4, Exercise 2.74

(define (makeFromRealImag x y)
  (define (dispatch op)
    (cond ((eq? op 'realPart) x)
          ((eq? op 'imagPart) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: makeFromRealImag" op))))
  dispatch)

(define (applyGeneric op arg) (arg op))

(define (makeFromMagAng r a)
  (define (dispatch op)
    (cond ((eq? op 'realPart) (* (cos a) r))
          ((eq? op 'imagPart) (* (sin a) r))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op: makeFromMagAng" op))))
  dispatch)





;--------------------------------------------------------------
;Utility functions for this section:

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (addend exp)
  (car exp))
(define (augend exp)
  (cadr exp))


(define (multiplier exp)
  (car exp))
(define (multiplicand exp)
  (cadr exp))



;note: '(rectangular) to allow a map procedure onto the args (map typeTag args), which will produce a list
;in the future, this allows for procedures with multiple arguments, not all of  the same type

