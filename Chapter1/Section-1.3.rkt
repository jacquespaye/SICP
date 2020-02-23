#lang scheme


;--------------------------------------------------------------
;Section 1.3, Exercise 1.29

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (nextIndex z) (+ z 1))
  (define (coeff c)
    (cond ((or (= c 0) (= c n)) 1)
          ((isEven c) 2)
          (else 4)))
  (define (term u) (* (coeff u) (f (+ a (* u h)))))
  (* (/ h 3.0) (sum term 0 nextIndex n)))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.30
(define (sumIter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.31
(define (factorial n)
  (product identity 1 inc n))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.32
(define (accumulate combiner nullValue term a next b)
  (if (> a b)
      nullValue
      (combiner (term a) (accumulate combiner nullValue term (next a) next b))))

;to calculate product: (accumulate * 1 term 1 next 10)
;to calculate sum:     (accumulate + 1 term 1 next 10)

(define (accIter combiner nullValue term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a nullValue))



;--------------------------------------------------------------
;Section 1.3, Exercise 1.33
(define (accFilter combiner nullValue filter term a next b)
  (define (filteredTerm x)
    (cond ((= x 0) 0)
          ((filter x) (term x))
          (else nullValue)))
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (filteredTerm a) result))))
  (iter a nullValue))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.35,1.36
(define (search f negPoint posPoint)
  (let ((midpoint (average negPoint posPoint)))
    (if (closeEnough? negPoint posPoint)
        midpoint
        (let ((testValue (f midpoint)))
          (cond
            ((positive? testValue)
             (search f negPoint midpoint))
            ((negative? testValue)
             (search f midpoint posPoint))
            (else midpoint))))))

(define (average a b) (/ (+ a b) 2.0))

(define (closeEnough? a b)
  (< (abs (- a b)) 0.001))


(define (halfInterval f a b)
  (let ((u (f a))
        (v (f b)))
    (cond ((and (negative? u) (positive? v))
           (search f a b))
          ((and (negative? v) (positive? u))
           (search f b a))
          (else (error "Values are not of opposite sign--" a b)))))

(define tolerance 0.0001)

(define (fixedPoint f firstGuess)
  (define (closeEnough a b)
    (< (abs (- a b)) tolerance))
  (define (try g nTries)
    (display g)
    (newline)
    (let ((next (f g)))
      (cond ((closeEnough g next) next)
            ((> nTries 200) (error "has not converged after 200 iterations") )
            (else (try next (+ nTries 1)))
            )))
  (try firstGuess 1))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.37

(define (squareRoot n)
  (fixedPoint (lambda (y) (average y (/ n y))) 1.0))


(define (contFrac n d k)
  (define (cfCalc i)
    (if (= i k) (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (cfCalc (+ i 1))))))
  (cfCalc 1))

(define (contFracIter n d k)
  (define (cfCalc i result)
    (if (= i 0) result
    (cfCalc (- i 1) (/ (n i) (+ (d i)
                                result)))))
  (cfCalc (- k 1) (/ (n k) (d k))))



;--------------------------------------------------------------
;Section 1.3, Exercise 1.38

(define (e n)
  (define (dIndex i)
    (if (= (remainder (+ i 1) 3) 0)
        (* (/ (+ i 1) 3) 2)
        1))
  (+ 2 (contFracIter (lambda (i) 1.0) dIndex n)))



;--------------------------------------------------------------
;Section 1.3, Exercise 1.39
(define (tanCF x k)
  (define (nIndex i)
    (if (= i 1) x (- (square x))))
  (define (dIndex i)
    (- (* 2 i) 1.0))
  (contFracIter nIndex dIndex k))





;--------------------------------------------------------------
;Section 1.3, Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.43
(define (repeated f n)
  (cond ((= n 0) identity)
        ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.44
(define (smooth f)
  (define (average a b c) (/ (+ a b c) 3))
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

;to smooth repeatedly:
;((repeated smooth n) f)

(define (nFoldSmooth f n)
  ((repeated smooth n) f))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.45


(define (nRoot x n)
    (fixedPoint ((repeated averageDamp (floor (log2 n)))
                 (lambda (y) (/ x (expt y (- n 1))))) 1.0))


;--------------------------------------------------------------
;Section 1.3, Exercise 1.46

(define (iterativeImprove goodEnough improveGuess)
  (define (iterHelper g)
    (if (goodEnough g) g
        (iterHelper (improveGuess g))))
  iterHelper)

(define (iterSqrt x)
  (define (goodEnough a)
    (< (abs (- (square a) x)) 0.0001))
  (define (improveGuess y)
    (average y (/ x y)))
  ((iterativeImprove goodEnough improveGuess) 1.0))

(define (fixedIter f g)
  (define (goodEnough a)
    (< (abs (- a (f a))) 0.0001))
  ((iterativeImprove goodEnough f) g))

;--------------------------------------------------------------
;Utility functions for this section:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(define (piSum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (term a) (f (+ a (/ dx 2))))
  (define (next a) (+ a dx))
  (* (sum term a next b) dx))

(define (isEven z) (= (remainder z 2) 0))


(define (identity x) x)
    
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (approxPi n)
  (define (piNext x) (+ x 2))
  (define (piTerm x) (* x (+ x 2)))
  (* 4.0 (/ (prodIter piTerm 2 piNext n)
            (prodIter square 3 piNext (+ n 1)))))

(define (altPi n)
  (define (term n)
    (/ (* 4.0 (square n))
       (- (* 4.0 (square n)) 1)))
  (* 2 (product term 1 inc n)))


(define (prodIter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (cleanRandom n)
  (if (< n 4294967088) (random n)
      (floor (* (random 4294967087) (/ n 4294967087)))))

(define (squareCheck a n)
  (cond ((= a 1) (square a))
        ((= a (- n 1)) (square a))
        ((= (remainder (square a) n) 1) 0)
        (else (square a))))

(define (expMod base exp m)
  (cond ((= exp 0) 1)
        ((isEven exp)
         (remainder (squareCheck (expMod base (/ exp 2) m) m) m))
        (else (remainder (* base (expMod base (- exp 1) m)) m))))

(define (rabinTest n t)
  (cond ((= t 0) #t)
        ((= n 1) #f)
        ((not (= (expMod (+ (cleanRandom (- n 1)) 1) (- n 1) n) 1)) #f)
        (else (rabinTest n (- t 1)))))

(define (prime? x)
  (rabinTest x 5))

(define (primeSquareSum a b)
  (accFilter + 0 prime? square a inc b))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (relativePrimeProduct n)
  (define (relativePrime? x)
    (= (gcd x n) 1))
  (accFilter * 1 relativePrime? identity 1 inc (- n 1)))


(define dx 0.00001)

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx)))

(define (newtonSquareRoot x)
  (newtonSearch (lambda (y) (- (square y)
                               x)) 1.0))

(define (fixedPointOfTransform f transform guess)
  (fixedPoint (transform f) guess))

(define (squareRootGeneral x)
  (fixedPointOfTransform
   (lambda (y) (/ x y))
   averageDamp
   1.0))

(define (squareRootNewtonGeneral x)
  (fixedPointOfTransform
   (lambda (y) (- (square y) x))
   newtonTransform
   1.0))

(define (newtonTransform f)
  (lambda (x) (- x
                 (/ (f x)
                    ((deriv f) x)))))

(define (newtonSearch f guess)
  (fixedPoint (newtonTransform f) guess))


(define (averageDamp f)
  (lambda (x) (average x (f x))))

(define (squareRoot2 x)
  (fixedPoint (averageDamp (lambda (y) (/ x y))) 1.0))

(define (cubeRoot x)
  (fixedPoint (averageDamp (lambda (y) (/ x (square y)))) 1.0))

(define (log2 n)
  (/ (log n) (log 2)))

