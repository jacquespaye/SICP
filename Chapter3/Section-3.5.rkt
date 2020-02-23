#lang sicp


;--------------------------------------------------------------
;Section 3.4, Exercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

;--------------------------------------------------------------
;Section 3.4, Exercise 3.54

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (integers-starting-from 2)
                              factorials)))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.55
(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams
    (partial-sums s)
    (stream-cdr s))))

;--------------------------------------------------------------
;Section 3.4, Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1
                          (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define S (cons-stream 1
                       (merge (scale-stream S 2)
                              (merge (scale-stream S 3)
                                     (scale-stream S 5)))))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.59


(define (integrate-series s)
  (define (iter s denom)
    (cons-stream
     (/ (stream-car s) denom)
     (iter (stream-cdr s)
           (+ denom 1))))
  (iter s 1))

(define exp-series
  (cons-stream
   1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
               (scale-stream
                (integrate-series sine-series)
                -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1)
      (stream-car s2))
   (add-streams
    (scale-stream
     (stream-cdr s2)
     (stream-car s1))
    (mul-series
     (stream-cdr s1)
     s2))))

;--------------------------------------------------------------
;Section 3.4, Exercise 3.61
(define (invert-unit-series s)
  (cons-stream
   1
   (scale-stream
    (mul-series
     (invert-unit-series s)
     (stream-cdr s))
    -1)))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.62

(define (div-series n d)
  (if (= (stream-car d) 0) (error "Denominator must not have 0 constant term")
  (mul-series n (scale-stream (invert-unit-series d) (/ 1 (stream-car d))))))

(define tangent-series
  (div-series sine-series cosine-series))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.64
(define (stream-limit s limit)
  (let ((s0 (stream-car s))
        (s1 (stream-cadr s)))
    (if (< (abs (- s1 s0))
           limit)
        s1
        (stream-limit (stream-cdr s) limit))))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.65

(define (ln2-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define ln2-stream-euler
  (euler-transform ln2-stream))

(define ln2-stream-accel
  (accelerated-sequence euler-transform ln2-stream))

;--------------------------------------------------------------
;Section 3.4, Exercise 3.67

(define (pairs-all s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x)
                   (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x)
                   (list x (stream-car t)))
                 (stream-cdr s)))
    (pairs-all (stream-cdr s)
               (stream-cdr t)))))



;--------------------------------------------------------------
;Section 3.4, Exercise 3.68

(define (louis-pairs s t)
  (interleave-2
   (stream-map
    (lambda (x)
      (list (stream-car s) x))
    t)
   (louis-pairs (stream-cdr s)
                (stream-cdr t))))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.69


(define (triples s t u)
  (cons-stream
   (list (stream-car s)
         (stream-car t)
         (stream-car u))
   (interleave
    (stream-map
     (lambda (x)
       (cons (stream-car s)
             x)) (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.70

(define (merge-weighted s1 s2 weighter)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (s1weight (weighter s1car))
                (s2weight (weighter s2car)))
           (cond ((<= s1weight s2weight)
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1)
                                   s2 weighter)))
                 (else
                  (cons-stream
                   s2car
                   (merge-weighted s1
                                   (stream-cdr s2) weighter))))))))

(define (pairs-weighted s t weighter)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s)
                    (stream-cdr t)
                    weighter)
    weighter)))

(define (i-plus-j pair)
  (+ (car pair)
     (cadr pair)))

(define (235-pair pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))


(define (not-div number divisors)
  (cond ((null? divisors) #t)
        ((= (remainder number (car divisors)) 0) #f)
        (else (not-div number (cdr divisors)))))


(define not-235-integers
  (stream-filter (lambda (x) (not-div x '(2 3 5))) integers))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.71


(define ramanujan-pairs
  (two-consec-stream sum-cubes pairs-by-sum-cubes)) ;use custom-display to show these

(define (multiple-display . items)
  (if (not (null? items))
      (begin (display (car items))
             (apply multiple-display (cdr items)))))

(define (print-ramanujan rama-pair)
  (let ((way-one (car rama-pair))
        (way-two (cadr rama-pair)))
    (define (print-cube pair)
      (multiple-display (car pair) "^3 + " (cadr pair) "^3 = " (sum-cubes pair)))
    (print-cube way-one)
    (newline)
    (print-cube way-two)
    (newline) (newline)))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.72

;I decided to implement this generically for any exponent and any number of ways

(define (taxicab exponent ways)
  (define (weight pair)
    (+ (expt (car pair) exponent)
       (expt (cadr pair) exponent)))
  (define (make-list stream ways)
    (define (iter stream ways result)
      (if (= ways 0) (reverse result) ; reverse for convenience, adds some processing time
          (iter (stream-cdr stream) (- ways 1) (cons (stream-car stream) result))))
    (iter stream ways '()))
  (define (all-equal? list)
    (cond ((null? (cdr list)) #t)
          ((= (car list)
              (cadr list)) (all-equal? (cdr list)))
          (#t #f)))
  (define (consecutive-comparison stream)
    (let ((candidate-list (make-list stream ways)))
      (if (all-equal? (map weight candidate-list))
          (cons-stream
           (cons (weight (car candidate-list)) candidate-list)
           (consecutive-comparison (stream-cdr stream)))
          (consecutive-comparison (stream-cdr stream)))))
  (consecutive-comparison (pairs-weighted integers integers weight)))



;--------------------------------------------------------------
;Section 3.4, Exercise 3.73


(define (RC R C dt)
  (define (output-proc s v0)
    (add-streams
     (scale-stream s R)
     (integral (scale-stream s (/ 1 C)) v0 dt)))
  output-proc)


;--------------------------------------------------------------
;Section 3.4, Exercise 3.74
(define zero-crossings-2
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))




;--------------------------------------------------------------
;Section 3.4, Exercise 3.75
(define (sign x)
  (cond ((>= x 0) 1)
        (#t 0)))

(define (sign-change-detector new-value old-value)
  (- (sign new-value) (sign old-value)))

(define (make-zero-crossings
         input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define sense-data (cons-stream 1 (cons-stream 2 (cons-stream -1 (cons-stream -4 (cons-stream 4 (cons-stream 5 '())))))))

(define zero-crossings
  (make-zero-crossings sense-data 0))



;--------------------------------------------------------------
;Section 3.4, Exercise 3.76

(define smooth-test
  (list->stream '(1 -5 3 4 5 -1 4 3 -1 5 -1 -4 -4 -3 -2 -7 3 5)))


(define (smooth-2 s)
  (if (stream-null? (stream-cdr s))
      the-empty-stream
      (cons-stream
       (average (stream-car s)
                (stream-cadr s))
       (smooth (stream-cdr s)))))

(define (smooth s)
  (stream-map
   average
   (stream-cdr s) s))

(define (smooth-crossings-mod stream)
  (let ((smoothed (smooth stream)))
    (stream-map sign-change-detector
                smoothed
                (cons-stream 0 smoothed))))

(define (make-smooth-crossings
         input-stream last-value last-avpt)
  (let ((avpt
         (/ (+ (stream-car input-stream)
               last-value)
            2)))
    (cons-stream
     (sign-change-detector avpt last-avpt)
     (make-smooth-crossings
      (stream-cdr input-stream) (stream-car input-stream) avpt))))



;--------------------------------------------------------------
;Section 3.4, Exercise 3.77

(define (integral-new
         delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral-new
          (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand))
             initial-value)
          dt)))))


(define (delayed-integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand
            (force delayed-integrand)))
       (add-streams
        (scale-stream integrand dt)
        int))))
  int)

;--------------------------------------------------------------
;Section 3.4, Exercise 3.78

(define (solve f y0 dt)
  (let ((y '()) (dy '()))
    (set! y (integral-new (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(define (solve-2nd a b dt y0 dy0)
  (let ((y '()) (dy '()) (ddy '()))
    (set! dy (delayed-integral (delay ddy) dy0 dt))
    (set! y (delayed-integral (delay dy) y0 dt))
    (set! ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
    y))


;--------------------------------------------------------------
;Section 3.4, Exercise 3.79

(define (general-2nd f dy0 y0 dt)
  (let ((y '()) (dy '()) (ddy '()))
    (set! dy (delayed-integral (delay ddy) dy0 dt))
    (set! y (delayed-integral (delay dy) y0 dt))
    (set! ddy (stream-map f dy y))
    y))

;--------------------------------------------------------------
;Section 3.4, Exercise 3.80
(define (RLC R L C dt)
  (define (output-proc vC0 iL0)
    (let ((iL '()) (vC '()) (diL '()) (dvC '()))
      (set! iL (delayed-integral (delay diL) iL0 dt))
      (set! vC (delayed-integral (delay dvC) vC0 dt))
      (set! dvC (scale-stream iL (/ -1 C)))
      (set! diL (add-streams
                 (scale-stream vC (/ L))
                 (scale-stream iL (* -1 (/ R L)))))
      (cons vC iL)))
  output-proc)




;--------------------------------------------------------------
;Section 3.4, Exercise 3.81
(define (rand-update x)
  (let ((a 16807)
        (b 0)
        (m (- (expt 2 31) 1)))
    (remainder
     (+
      (* a x)
      b)
     m)))

(define random-numbers
  (cons-stream 1
               (stream-map rand-update random-numbers)))



;--------------------------------------------------------------
;Section 3.4, Exercise 3.82


(define (estimate-integral P x1 x2 y1 y2)
  ;takes P(x,y) and bounds for rectangle to try
  (define result-stream
    (stream-map (lambda (list) (apply P list)) (point-stream x1 x2 y1 y2)))
  (scale-stream
   (monte-carlo result-stream 0.0 0)
   (* (abs (- x2 x1))
      (abs (- y2 y1)))))
       



;--------------------------------------------------------------
;Utility functions for this section:


(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp)
     (memo-proc
      (lambda () exp)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))


(define (square z) (* z z))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))




(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
              (stream-for-each proc
                               (stream-cdr s)))))

(define (custom-display display-proc s limit)
  (display-proc (stream-car s))
  (if (not (= limit 1))
      (custom-display display-proc (stream-cdr s) (- limit 1))))



(define (display-stream s . limit)
  (define (display-iter s rowsLeft)
    (display-line (stream-car s))
    (if (not (= rowsLeft 1))
        (display-iter (stream-cdr s) (- rowsLeft 1)))) 
  
  (if (null? limit)
      (stream-for-each display-line s)
      (display-iter s (car limit))))

(define (display-line x)
  (display x)
  (newline))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
               (cons-stream
                (stream-car stream)
                (stream-filter
                 pred
                 (stream-cdr stream))))
        (else (stream-filter
               pred
               (stream-cdr stream)))))



(define (integers-starting-from n)
  (cons-stream
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x)
                   (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible?
                   x (stream-car stream))))
           (stream-cdr stream)))))
(define primes-2
  (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))



(define integers-2
  (cons-stream 1 (add-streams ones integers-2)))

(define fibonacci
  (cons-stream
   0 (cons-stream
      1 (add-streams
         (stream-cdr fibonacci) fibonacci))))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define double
  (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2 (stream-filter
      prime-2? (integers-starting-from 3))))

(define (prime-2? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))







(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den)
           den
           radix)))





(define (square-series s)
  (mul-series s s))

(define (sum-stream s n)
  (define (iter s result n)
    (if (= n 0) result
        (iter (stream-cdr s) (+ result (stream-car s)) (- n 1))))
  (iter s 0.0 n))







(define (eval-powers series x n)
  (define (iter series x pwr result)
    (if (= pwr n) result
        (iter (stream-cdr series) x (+ pwr 1) (+ result
                                                 (* (stream-car series)
                                                    (expt x pwr))))))
  (iter series x 0 0.0))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2.0))


(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)


(define (pi-summands n)
  (cons-stream
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream
   (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream
     (- s2
        (/
         (square (- s2 s1))
         (+ (- s0 (* 2 s1))
            s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (stream-cadr s)
  (stream-car (stream-cdr s)))



(define (sqrt-new x tolerance)
  (stream-limit (sqrt-stream x) tolerance))



(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (place-old u v)
    (+ (expt 2 (- u 1))
       -2
       (* (expt 2 (- u 1))
          (+ (+ v 1 (* u -1))
             (max (+ v (* -1 u)
                     -1) 0)))))

(define (place u v)
  (-
   (* (expt 2 (- u 1))
      (max (+ (* 2 v) (- (* 2 u)) 1)
           (+ v (- u) 2)))
   2))

(define (sci-not n)
  (define (iter n power)
    (if (< n 10) (begin (display n)
                        (display " e^")
                        (display power))
        (iter (/ n 10.0) (+ power 1))))
  (iter n 0))






(define (interleave-3 s1 s2)
  (interleave-2 s1 s2))

(define-syntax interleave-2
  (syntax-rules ()
    ((interleave-2 s1 s2)
     (cons-stream
      (stream-car s1)
      (interleave-3 s2 (stream-cdr s1))))))



(define (pythagorean? triple)
  (= (square (caddr triple))
     (+ (square (cadr triple))
        (square (car triple)))))



(define (cube x) (* x x x))

(define (sum-cubes pair)
  (+ (cube (car pair))
     (cube (cadr pair))))

(define (stream-cddr s)
  (stream-cdr (stream-cdr s)))

(define (two-consec-stream weight s)
  (if (= (weight (stream-car s)) (weight (stream-cadr s)))
      (cons-stream (list (stream-car s)
                         (stream-cadr s)) (two-consec-stream weight (stream-cdr s)))
      (two-consec-stream weight (stream-cdr s))))

(define pairs-by-sum-cubes
  (pairs-weighted integers integers sum-cubes))






(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)









(define (list->stream list)
  (define (iter list result)
    (if (= (length list) 0) result
        (iter (cdr list) (cons-stream (car list) result))))
  (iter (reverse list) '()))







(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s)
      (stream-cadr s))
   (map-successive-pairs f (stream-cddr s))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))


(define (monte-carlo experiment-stream
                     passed
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream)
      passed
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6.0 p)))
   (monte-carlo cesaro-stream 0 0)))

(define (stream-caar x)
  (stream-car (stream-car x)))

(define (seed-random-old request-stream seed)
  (define (next request-stream new-value)
    (cond ((stream-null? request-stream) the-empty-stream)
          ((and (symbol? (stream-car request-stream))
                (eq? (stream-car request-stream) 'generate))
             (cons-stream
              new-value
              (next (stream-cdr request-stream) (rand-update new-value))))
          ((and (pair? (stream-car request-stream))
                (eq? (car (stream-car request-stream)) 'reset))
           (cons-stream
            new-value
            (next (stream-cdr request-stream) (cdr (stream-car request-stream)))))
          (else (error "Unrecognized input:" (stream-car request-stream)))))
  (next request-stream seed))

(define (seed-random request-stream seed)
  (define s
    (cons-stream
     seed
     (stream-map
      (lambda (request number)
        (cond ((eq? request 'generate) (rand-update number))
              ((and (pair? request)
                    (eq? (car request) 'reset)) (cdr request))
              (else (error "Invalid request to seed-random:" request))))
      request-stream s)))
  s)


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (* 1.0 range)))))

(define (point-stream x1 x2 y1 y2)
  (cons-stream (list (random-in-range x1 x2)
                     (random-in-range y1 y2))
               (point-stream x1 x2 y1 y2)))


    
  

