#lang scheme


;--------------------------------------------------------------
;Section 2.1, Exercise 2.1
(define (makeRat n d)
  (if (< d 0)
      (cons (* n -1) (* d -1))
      (cons n d)))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.2
(define (makePoint x y)
  (cons x y))

(define (xPoint p)
  (car p))

(define (yPoint p)
  (cdr p))

(define (makeSegment a b)
  (cons a b))

(define (startSegment s)
  (car s))

(define (endSegment s)
  (cdr s))

(define (average a b)
  (/ (+ a b) 2.0))

(define (midpoint s)
  (makePoint (average (xPoint (startSegment s))
                      (xPoint (endSegment s)))
             (average (yPoint (startSegment s))
                      (yPoint (endSegment s)))))

(define (printPoint p)
  (display "(")
  (display (xPoint p))
  (display ",")
  (display (yPoint p))
  (display ")"))



;--------------------------------------------------------------
;Section 2.1, Exercise 2.3

;rectangle implemented as two perpendicular segments intersecting at a point
(define (makeRectangle s1 s2)
  (cons s1 s2))

(define (p1 r)
  (car (car r)))

(define (p2 r)
  (cdr (car r)))

(define (p3 r)
  (cdr (cdr r)))

(define (p4 r)
  (makePoint (+ (xPoint (p3 r))
                (- (xPoint (p1 r)) (xPoint (p2 r))))
             (+ (yPoint (p3 r))
                (- (yPoint (p1 r)) (yPoint (p2 r))))))
(define (perimeter r)
  (+ (* 2 (distance (p1 r) (p2 r)))
     (* 2 (distance (p2 r) (p3 r)))))

(define (distance p1 p2)
  (sqrt (+ (square (- (xPoint p1)
                      (xPoint p2)))
           (square (- (yPoint p1)
                      (yPoint p2))))))

(define (square a) (* a a))

(define (area r)
  (* (distance (p1 r) (p2 r))
     (distance (p2 r) (p3 r))))

;rectangle implemented as two diagonals
(define (makeRectangle s1 s2)
  (cons s1 s2))

(define (p1 r)
  (car (car r)))

(define (p2 r)
  (cdr (cdr r)))

(define (p3 r)
  (cdr (car r)))

(define (p4 r)
  (car (cdr r)))



;--------------------------------------------------------------
;Section 2.1, Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.5
(define (countFactor number factor)
  (define (countIter number factor n)
    (if (= (remainder number factor) 0)
        (countIter (/ number factor) factor (+ n 1))
        n))
  (countIter number factor 0))

(define (logn n a)
  (/ (log a) (log n)))

(define (cons25 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car25 z)
  (countFactor z 2))

(define (cdr25 z)
  (countFactor z 3))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))



;evaluation process
(add-1 zero)
(lambda (f) (lambda (x) (f ((n f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)));this is "one"

(add-1 one)
(lambda (f) (lambda (x) (f ((n f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x)))); this is "two"

(add-1 two)
(lambda (f) (lambda (x) (f ((n f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f (f x)))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f (f x))) x))))
(lambda (f) (lambda (x) (f (f (f x))))); this is "three"

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.7

(define (makeInterval a b)
  (cons a b))

(define (upperBound z)
  (cdr z))

(define (lowerBound z)
  (car z))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.8
(define (subInterval x y)
  (makeInterval (- (lowerBound x)
                   (upperBound y))
                (- (upperBound x)
                   (lowerBound y))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.9
(define (width i)
  (/ (- (upperBound i)
        (lowerBound i))
     2))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.10
(define (divInterval x y)
  (if (or (< (* (lowerBound x) (upperBound x)) 0)
          (< (* (lowerBound y) (upperBound y)) 0))
      (error "At least one interval spans 0")
      (mulInterval x (makeInterval
                      (/ 1.0 (upperBound y))
                      (/ 1.0 (lowerBound y))))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.11
(define (mulInterval2 x y)
  (let ((a (car x))
        (b (cdr x))
        (c (car y))
        (d (cdr y)))
    (cond ((and (>= a 0) (>= b 0) (>= c 0) (>= d 0))
           (makeInterval (* a c) (* b d)))
          ((and (<= a 0) (<= b 0) (<= c 0) (<= d 0))
           (makeInterval (* b d) (* a c)))
          ((and (>= a 0) (>= b 0) (<= c 0) (<= d 0))
           (makeInterval (* b c) (* a d)))
          ((and (<= a 0) (<= b 0) (>= c 0) (>= d 0))
           (makeInterval (* a d) (* b c)))
          ((and (>= a 0) (>= b 0) (<= c 0) (>= d 0))
           (makeInterval (* b c) (* b d)))
          ((and (<= a 0) (>= b 0) (>= c 0) (>= d 0))
           (makeInterval (* a d) (* b d)))
          ((and (<= a 0) (<= b 0) (<= c 0) (>= d 0))
           (makeInterval (* a d) (* a c)))
          ((and (<= a 0) (>= b 0) (<= c 0) (<= d 0))
           (makeInterval (* b c) (* a c)))
          ((and (<= a 0) (>= b 0) (<= c 0) (>= d 0))
           (makeInterval (min (* a d)
                              (* b c))
                         (max (* a c)
                              (* b d)))))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.12
(define (makeCenterPercent c p)
  (let ((gap (* (/ p 100.0) (abs c))))
  (makeInterval (- c gap)
                (+ c gap)))); or (makeCenterWidth c gap)


;--------------------------------------------------------------
;Section 2.1, Exercise 2.13
(define (par1 r1 r2)
  (divInterval
   (mulInterval2 r1 r2)
   (addInterval r1 r2)))

(define (par2 r1 r2)
  (let ((one (makeInterval 1 1)))
    (divInterval
     one
     (addInterval (divInterval one r1)
                  (divInterval one r2)))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.14
(define a (makeCenterPercent 10 1))
(define b (makeCenterPercent 4 1))
(define c (makeCenterPercent 1 0))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.15

;multiplying and dividing two uncertain intervals doubles the uncertainty, whereas adding keeps it the same




;--------------------------------------------------------------
;Utility functions for this section:

(define (percent i)
  (* 100 (/ (width i) (abs (center i)))))


(define (addRat x y)
  (makeRat (+ (* (numer x) (denom y))
              (* (numer y) (denom x)))
           (* (denom x) (denom y))))

(define (subRat x y)
  (makeRat (- (* (numer x) (denom y))
              (* (numer y) (denom x)))
           (* (denom x) (denom y))))

(define (mulRat x y)
  (makeRat (* (numer x) (numer y))
           (* (denom x) (denom y))))

(define (divRat x y)
  (makeRat (* (numer x) (denom y))
           (* (denom x) (numer y))))

(define (equalRat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (printRat x)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define a (makePoint 2 4))
(define b (makePoint 0 -2))
(define d1 (makeSegment b a))
(define c (makePoint -2 0))
(define d (makePoint 4 1))
(define d2 (makeSegment d c))

(define r (makeRectangle d1 d2))


(define (addInterval x y)
  (makeInterval (+ (lowerBound x)
                   (lowerBound y))
                (+ (upperBound x)
                   (upperBound y))))
               

(define (mulInterval x y)
  (let ((p1 (* (lowerBound x)
               (lowerBound y)))
        (p2 (* (lowerBound x)
               (upperBound y)))
        (p3 (* (upperBound x)
               (lowerBound y)))
        (p4 (* (upperBound x)
               (upperBound y))))
    (makeInterval (min p1 p2 p3 p4)
                  (max p1 p2 p3 p4))))



(define (makeCenterWidth c w)
  (makeInterval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lowerBound i)
        (upperBound i))
     2))

(define (evaluate e x y)
  (let ((p1 (e (lowerBound x)
               (lowerBound y)))
        (p2 (e (lowerBound x)
               (upperBound y)))
        (p3 (e (upperBound x)
               (lowerBound y)))
        (p4 (e (upperBound x)
               (upperBound y))))
    (makeInterval (min p1 p2 p3 p4)
                  (max p1 p2 p3 p4))))

  
