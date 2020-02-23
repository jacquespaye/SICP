#lang scheme

;--------------------------------------------------------------
;Section 1.2, Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


;--------------------------------------------------------------
;Section 1.2, Exercise 1.11

(define (fRecursive n)
  (if (< n 3)
      n
      (+ (* 1 (fRecursive (- n 1)))
         (* 2 (fRecursive (- n 2)))
         (* 3 (fRecursive (- n 3))))))

;--------------------------------------------------------------
;Section 1.2, Exercise 1.12
(define (pascal l e)
  (cond ((= e 1) 1)
        ((= e l) 1)
        ((< e 1) (printf "Error, element index out of range"))
        ((> e l) (printf "Error, element index out of range"))
        (else (+ (pascal (- l 1) e)
           (pascal (- l 1) (- e 1))))))

;--------------------------------------------------------------
;Section 1.2, Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))



;--------------------------------------------------------------
;Section 1.2, Exercise 1.16
(define (fastExp b n)
  (cond ((= n 0)
         1)
        ((isEven n)
         (square (fastExp b (/ n 2))))
        (else (* b
                 (fastExp b (- n 1))))))



;--------------------------------------------------------------
;Section 1.2, Exercise 1.17
(define (double n) (+ n n))
(define (halve n)
  (if (isEven n) (/ n 2)
      (printf "Error")))
(define (multFast a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((isEven b) (multFast (double a) (halve b)))
        (else (+ a (multFast a (- b 1))))))


;--------------------------------------------------------------
;Section 1.2, Exercise 1.18
(define (multIter a b)
  (multIterator a b 0))

(define (multIterator a b c)
  (cond ((= b 0) c)
        ((isEven b) (multIterator (double a) (halve b) c))
        (else (multIterator a (- b 1) (+ c a)))))



;--------------------------------------------------------------
;Section 1.2, Exercise 1.19
(define (fastFib n)
  (fastFibIter 1 0 0 1 n))

(define (fastFibIter a b p q count)
  (cond ((= count 0) b)
        ((isEven count)
         (fastFibIter a
                      b
                      (+ (square p) (square q))
                      (+ (* 2 p q) (square q))
                      (/ count 2)))
        (else
         (fastFibIter (+ (* b q)
                         (* a q)
                         (* a p))
                      (+ (* b p)
                         (* a q))
                      p
                      q
                      (- count 1)))))


;--------------------------------------------------------------
;Section 1.2, Exercise 1.21, 1.23

(define (next n)
  (if (= n 2) 3
      (+ n 2)))

(define (smallestDivisor n)
  (findDivisor n 2))

(define (findDivisor n t)
  (cond ((> (square t) n) n)
        ((divides? n t)
         t)
        (else (findDivisor n (next t)))))

(define (divides? a b)
  (= (remainder a b) 0))



;--------------------------------------------------------------
;Section 1.2, Exercise 1.22
(define (timedPrime? n)
  (startPrimeTest n (current-milliseconds)))

(define (startPrimeTest n startTime)
  (if (prime? n)
      (reportPrime n (- (current-milliseconds) startTime))
      (values)))

(define (reportPrime n elapsedTime)
  (newline)
  (display n)
  (display "***PRIME*** ")
  (display elapsedTime))

(define (testPrimeRange a b)
  (if (isEven a)
      (primeRangeTester a b)
      (primeRangeTester (+ a 1) b)))

(define (primeRangeTester a b)
  (if (> a b) (display "  DONE")
      (testIndividualPrime a b)))

(define (testIndividualPrime a b)
  (timedPrime? a)
  (primeRangeTester (+ a 1) b))


;--------------------------------------------------------------
;Section 1.2, Exercise 1.24
(define (expMod base exp m)
  (cond ((= exp 0) 1)
        ((isEven exp)
         (remainder (square (expMod base (/ exp 2) m))m))
        (else (remainder (* base (expMod base (- exp 1) m)) m))))

(define (fermatTest n)
  (define (try a)
    (= (expMod a n n) a))
  (try (+ 1 (random (- n 1)))))

(define (fastPrime? n times)
  (cond ((= times 0) true)
        ((fermatTest n)
         (fastPrime? n (- times 1)))
        (else false)))


;--------------------------------------------------------------
;Section 1.2, Exercise 1.27

(define (carmic n)
  (carmicIter n (- n 1)))

(define (carmicIter n a)
  (cond ((= a 1) #t)
        ((= (expMod a n n) a) (carmicIter n (- a 1)))
        (else #f)))


;--------------------------------------------------------------
;Section 1.2, Exercise 1.28
(define (rabinTest n t)
  (cond ((= t 0) #t)
        ((not (= (expMod (+ (cleanRandom (- n 1)) 1) (- n 1) n) 1)) #f)
        (else (rabinTest n (- t 1)))))






;--------------------------------------------------------------
;Utility functions for this section:

(define (cleanRandom n)
  (if (< n 4294967088) (random n)
      (floor (* (random 4294967087) (/ n 4294967087)))))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factCount n)
  (factIter 1 1 n))

(define (factIter product counter maxCount)
  (if (> counter maxCount)
      product
      (factIter (* counter product)
                (+ counter 1)
                maxCount)))



(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fibIter a b n)
  (if (= n 0)
      b
      (fibIter (+ a b) a (- n 1))))


(define (countChange amount)
  (cc amount 5))

(define (cc amount kindsCoins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kindsCoins 0))
         0)
        (else
         (+ (cc amount (- kindsCoins 1))
            (cc (- amount (firstDenom kindsCoins))
                kindsCoins)))))

(define (firstDenom kindsCoins)
  (cond ((= kindsCoins 1) 1)
        ((= kindsCoins 2) 5)
        ((= kindsCoins 3) 10)
        ((= kindsCoins 4) 25)
        ((= kindsCoins 5) 50)))


(define (fIter n)
  (if (< n 3)
      n
      (fIterator 0 1 2 2 n)))

(define (fIterator x y z c n)
  (if (= c n)
      z
      (fIterator y z (+ z (* 2 y) (* 3 x)) (+ c 1) n)))

(define (expRecursive b n)
  (if (= n 0) 1
      (* b (expRecursive b (- n 1)))))

(define (expIterative b n)
  (expIterator 1 b n))

(define (expIterator result base counter)
  (if (= counter 0) result
      (expIterator (* result base) base (- counter 1))))


(define (square n)
  (* n n))

(define (isEven n)
  (= (remainder n 2) 0))

(define (fastExpIter b n)
  (fastExpIterator 1 b n))

(define (fastExpIterator a b n)
  (cond ((= n 0) a)
        ((isEven n) (fastExpIterator a (square b) (/ n 2)))
        (else (fastExpIterator (* a b) b (- n 1)))))

(define (multRecursive a b)
  (if (= b 0)
      0
      (+ a (multRecursive a (- b 1)))))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))


(define (prime?  n)
  (= (smallestDivisor n) n))











