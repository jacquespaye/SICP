#lang scheme

;--------------------------------------------------------------
;Section 1.1, Exercise 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6 (/ 4 5)))))
   (* 3
      (- 6
         2)
      (- 2
         7)))

;--------------------------------------------------------------
;Section 1.1, Exercise 1.3:
;3max function problem
(define (square x) (* x x))

(define (sumOfSquares x y) (+ (square x) (square y)))


(define (3max x y z)
  (cond ((and (<= x y) (<= x z)) (sumOfSquares y z))
        ((and (<= y x) (<= y z)) (sumOfSquares x z))
        (else (sumOfSquares x y))))

;--------------------------------------------------------------
;Section 1.1, Exercise 1.4
(define (aPlusAbsB a b) ((if (> b 0) + -) a b))

;--------------------------------------------------------------
;Section 1.1, Exercise 1.5

(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))

(test 0 (p))

;under normal order eval, Scheme first evaluates the if statement, which evaluates to zero, thus returning zero
;under applicative order eval, scheme first evaluates 0 and (p), thus hanging in an infinite loop, which is what actually happens if code is run


;--------------------------------------------------------------
;Guessing square roots by Newton's Method:

(define (squareRoot x)

  (define accuracy 0.000001)
  
  (define (sqrtGuess g)
    (if (isGoodEnough g)
      g
      (sqrtGuess (improveGuess g))))
  
  (define (isGoodEnough g)
    (< (abs (- (square g) x)) accuracy))

  (define (square z) (* z z))

  (define (abs z)
    (if (> z 0) z (- z)))

  (define (improveGuess g)
    (average g (/ x g)))

  (define (average a b)
    (/ (+ a b) 2.0))

  (sqrtGuess 1)
)

;--------------------------------------------------------------
;Section 1.1, Exercise 1.6,1.7
;the percent difference method is implemented below (1.7), along with the 'new-if' procedure
;necessary to implement the test described in 1.6


(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


(define (squareRoot x)

  (define percentDiff 0.001)
  
  (define (sqrtGuess g)
    (new-if (isGoodEnough g)
      g
      (sqrtGuess (improveGuess g))))
  
  (define (isGoodEnough g)
    (< (abs (/ (- (improveGuess g) g) g)) percentDiff))

  (define (square z) (* z z))

  (define (abs z)
    (if (> z 0) z (- z)))

  (define (improveGuess g)
    (average g (/ x g)))

  (define (average a b)
    (/ (+ a b) 2.0))

  (sqrtGuess 1)
)

;-----problem--the new-if clause requires the two arguments to be evaluated, thus triggering an infinite loop


;--------------------------------------------------------------
;Section 1.1, Exercise 1.8

(define (cubeRoot x)

  (define percentDiff 0.001)
  
  (define (cubeGuess g)
    (if (isGoodEnough g)
      g
      (cubeGuess (improveGuess g))))
  
  (define (isGoodEnough g)
    (< (abs (/ (- (improveGuess g) g) g)) percentDiff))

  (define (cube z) (* z z z))

  (define (square z) (* z z))

  (define (abs z)
    (if (> z 0) z (- z)))

  (define (improveGuess g)
    (/ (+ (/ x
             (square g))
          (* 2 g))
       3)
    )

  (define (average a b)
    (/ (+ a b) 2.0))

  (cubeGuess 1.0)
)