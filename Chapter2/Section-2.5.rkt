#lang scheme

;--------------------------------------------------------------
;Section 2.5, Exercise 2.78

(define (attach-tag type-tag contents)
      (cons type-tag contents))

(define (type-tag datum)
  (cond ((integer? datum) 'integer)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((integer? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))


;--------------------------------------------------------------
;Section 2.5, Exercise 2.79
;see (put 'equ?) statements and the associated equal-rat and equal-complex procedures in the packages

;--------------------------------------------------------------
;Section 2.5, Exercise 2.80
;see (put '=zero?) statements and the associated =zero?-poly, =zero?-rat, and =zero?-complex procedures

;--------------------------------------------------------------
;Section 2.5, Exercise 2.81, 2.82, 2.84

(define (apply-generic op . args)
  (let ((type-tags  (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc
             (cond ((memq op '(raise project)) (apply proc (map contents args)))
                   ((and (memq 'rational type-tags)
                         (recursiveSearch 'polynomial args)) (apply proc (map contents args)))
                   (else (drop (apply proc (map contents args))))))
            ((not (allEqual type-tags));Answer for exercise 2.81
             (apply
              apply-generic op
              (raiseByList args
                           (getRaiseList args))))
            (else (error "No method for these types"
                         (list op type-tags)))))))


;--------------------------------------------------------------
;Section 2.5, Exercise 2.83

;see the statements with format (put 'raise '(type) raise) and the associated within the package installation code below



;--------------------------------------------------------------
;Section 2.5, Exercise 2.85
(define (levelsFromTop n)
  (define (levelsIter n result)
    (cond ((get 'raise (list (type-tag n)))
           (levelsIter (raise n) (+ result 1)))
          (else result)))
  (levelsIter n 0))

(define (drop n)
  (cond ((not (pair? n)) n)
        ((get 'project (list (type-tag n)))
         (let* ((projected (project n))
                (raised (raise projected)))
           (if (equ? n raised)
               (drop projected)
               n)))
        (else n)))


;--------------------------------------------------------------
;Section 2.5, Exercise 2.86

;See complex package below


;--------------------------------------------------------------
;Section 2.5, Exercise 2.87, 2.88

;See polynomial package below for =zero?-poly and negate-termlist procedures



;--------------------------------------------------------------
;Section 2.5, Exercise 2.89, 2.90

;See the polynomial package below for the integration of the two term-list representations

;--------------------------------------------------------------
;Section 2.5, Exercise 2.91

;See the div-terms procedure within the polynomial package below


;--------------------------------------------------------------
;Section 2.5, Exercise 2.92

;See the polynomial package below for this implementation, specifically the add-poly-new and mul-poly-new,
;which both use a new 'reorder' procedure to achieve addition and multiplication of polynomials in different
;variables

;--------------------------------------------------------------
;Section 2.5, Exercise 2.93, 2.94

;See the reduce-poly, remainder-terms, and gcd-poly procedures in the polynomial package below for the
;desired implementation

;--------------------------------------------------------------
;Section 2.5, Exercise 2.96

;See the pseudoremainder-terms procedure and the gcd-terms procedure in the polynomial package below


;--------------------------------------------------------------
;Section 2.5, Exercise 2.97

;See the reduce-terms and reduce-poly procedures in the polynomial package below



;--------------------------------------------------------------
;Below find all packages written in this section

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (sub-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: SUB-POLY" (list p1 p2))))
  (define (sub-terms L1 L2)
    (add-terms
     L1
     (negate-termlist L2)))

  (define (add-poly-new p1 p2)
    (let* ((order1 (reverse (traverseVars p1)))
           (order2 (reverse (traverseVars p2)))
           (combinedOrder (varUnion order1 order2))
           (p1Processed (cdr (reorder p1 combinedOrder)))
           (p2Processed (cdr (reorder p2 combinedOrder))))
        (make-poly
         (variable p1Processed)
         (add-terms (term-list p1Processed)
                    (term-list p2Processed)))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (add-terms L1 L2)
    ;we currently always default to sparse lists
    (cond ((empty-termlist? L1) (apply-generic 'make-sparse-term-list L2))
          ((empty-termlist? L2) (apply-generic 'make-sparse-term-list L1))
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1)
                                   L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms
                      L1
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term
                      (order t1)
                      (add (coeff t1)
                           (coeff t2)))
                     (add-terms
                      (rest-terms L1)
                      (rest-terms L2)))))))))
                      

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  
  (define (mul-poly-new p1 p2)
    (let* ((order1 (reverse (traverseVars p1)))
           (order2 (reverse (traverseVars p2)))
           (combinedOrder (varUnion order1 order2))
           (p1Processed (cdr (reorder p1 combinedOrder)))
           (p2Processed (cdr (reorder p2 combinedOrder))))
        (make-poly
         (variable p1Processed)
         (mul-terms (term-list p1Processed)
                    (term-list p2Processed)))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        the-empty-termlist
        (add-terms
         (mul-term-by-all-terms
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        the-empty-termlist
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms
            t1
            (rest-terms L))))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list the-empty-termlist
              the-empty-termlist)
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list the-empty-termlist L1)
              (let ((new-c (div (coeff t1)
                                (coeff t2)))
                    (new-o (- (order t1)
                              (order t2))))
                (let ((rest-of-result
                       (div-terms
                        (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2))
                        L2)))
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))))))))
  
  (define (getCoeffsIter terms result)
    (if (empty-termlist? terms) result
        (getCoeffsIter  (rest-terms terms) (cons (coeff (first-term terms)) result))))
  (define (divAllCoeffsIter terms number result)
    (if (empty-termlist? terms) result
        (adjoin-term
         (make-term
          (order (first-term terms))
          (div (coeff (first-term terms)) number))
         (divAllCoeffsIter (rest-terms terms) number result))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (divAllCoeffsIter a (apply gcd (getCoeffsIter a '()))
                          the-empty-termlist)
        (gcd-terms b (pseudoremainder-terms a b))))

  (define (reduce-terms n d)
    (if (or (empty-termlist? n)
            (empty-termlist? d))
        (error "Can't reduce terms, one of divisor/dividend is null:" (list n d))
        
        (let* ((g (gcd-terms n d))
               (c (coeff (first-term g)))
               (O2 (order (first-term g)))
               (O1 (max
                    (order (first-term n))
                    (order (first-term d))))
               (power (+ 1 (- O1 O2)))
               (factor (make-dense-term-list (expt c power)))
               (nDiv (car (div-terms (mul-terms n factor) g)))
               (dDiv (car (div-terms (mul-terms d factor) g)))
               (allCoeffs (append
                           (getCoeffsIter nDiv '())
                           (getCoeffsIter dDiv '())))
               (nn (divAllCoeffsIter nDiv (apply gcd allCoeffs) the-empty-termlist))
               (dd (divAllCoeffsIter dDiv (apply gcd allCoeffs) the-empty-termlist)))
          (list nn dd))))

  
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (let ((reduced (reduce-terms
                        (term-list p1)
                        (term-list p2))))
          (list
           (tag (make-poly (variable p1)
                           (car reduced)))
           (tag (make-poly (variable p1)
                           (cadr reduced)))))
        (error "Polys not in same variable: REDUCE-POLY" (list p1 p2))))              
      

  (define (remainder-terms a b)
    (cadr (div-terms a b)))

  (define (pseudoremainder-terms P Q)
    (if (or (empty-termlist? P)
            (empty-termlist? Q))
        (error "Can't find pseudoremainder, one of divisor/dividend is null:" (list P Q))
        (let ((O1 (order (first-term P)))
              (O2 (order (first-term Q)))
              (c (coeff (first-term Q)))
              (variable (variable P)))
          (cadr
           (div-terms
            (mul-terms P
                      (make-dense-term-list (expt c (+ 1 (- O1 O2)))))
            Q)))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1)
                       (variable p2))
        (make-poly
         (variable p1)
         (gcd-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: GCD" (list p1 p2))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (div-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))
              
  
  (define (=zero?-poly p)
    (define (=zero?-terms terms)
      (cond ((empty-termlist? terms) #t)
            ((=zero? (coeff
                      (first-term terms)))
             (=zero?-terms (rest-terms terms)))
            (else #f)))
    (=zero?-terms (term-list p)))                         

  (define (install-dense-package)
    (define (tag list) (attach-tag 'dense list))
    (define (make-dense-term-list terms)
      (tag terms))
    (define (make-sparse-term-list terms)
      (define (make-sparse-term-list-iter terms result)
        (cond ((null? terms) (attach-tag 'sparse (reverse result)))
              ((=zero? (car terms)) (make-sparse-term-list-iter (cdr terms) result))
              (else (make-sparse-term-list-iter (rest-terms terms)
                                                (cons (first-term terms) result)))))
      (make-sparse-term-list-iter terms '()))
    
    (define (adjoin-term term term-list)
      (cond ((=zero? (coeff term)) term-list)
            ((= (order term)
                (length term-list)) (cons (coeff term) term-list))
            ((> (order term)
                (length term-list)) (adjoin-term term (cons 0 term-list)))
            (else (error "Can't adjoin this term, likely not in order: TERM" term))))
    (define (first-term term-list) (list (- (length term-list) 1) (car term-list)))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list)
      (null? term-list))
    (define (negate-termlist term-list)
      (tag (map (lambda (term)
             (mul term -1))
           term-list)))
    (put 'adjoin-term '(term dense) (lambda (term term-list)
                                      (tag (adjoin-term term term-list))))
    (put 'make-dense-term-list '(dense) make-dense-term-list)
    (put 'make-sparse-term-list '(dense) make-sparse-term-list)
    (put 'first-term '(dense) first-term)
    (put 'rest-terms '(dense) (lambda (term-list) (tag (rest-terms term-list))))
    (put 'empty-termlist? '(dense) empty-termlist?)
    (put 'the-empty-termlist 'dense '(dense))
    (put 'negate-termlist '(dense) negate-termlist))

  (install-dense-package)

  (define (install-sparse-package)
    (define (tag list) (attach-tag 'sparse list))
    (define (make-sparse-term-list terms)
      (tag terms))
    (define (make-dense-term-list terms)
      (define (make-dense-term-list-iter terms result)
        (cond ((null? terms) (attach-tag 'dense result))
              ((= (length result) (caar terms))
               (make-dense-term-list-iter (cdr terms) (cons (cadar terms) result)))
              (else (make-dense-term-list-iter terms (cons 0 result)))))
      (make-dense-term-list-iter (reverse terms) '()))
    (define (adjoin-term term term-list)
      (if (=zero? (coeff term))
          term-list
          (cons term term-list)))
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list)
      (null? term-list))
    (define (negate-termlist term-list)
      (tag (map (lambda (term)
             (make-term
              (order term)
              (negate (coeff term))))
           term-list)))
    (put 'adjoin-term '(term sparse) (lambda (term term-list)
                                      (tag (adjoin-term term term-list))))
    (put 'make-sparse-term-list '(sparse) make-sparse-term-list)
    (put 'make-dense-term-list '(sparse) make-dense-term-list)
    (put 'first-term '(sparse) first-term)
    (put 'rest-terms '(sparse) (lambda (term-list) (tag (rest-terms term-list))))
    (put 'empty-termlist? '(sparse) empty-termlist?)
    (put 'the-empty-termlist 'sparse '(sparse))
    (put 'negate-termlist '(sparse) negate-termlist))
  (install-sparse-package)



  
  ;as of now we default to sparse
  (define the-empty-termlist (get 'the-empty-termlist 'sparse))
  (define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
  (define (adjoin-term term term-list)
    (apply-generic 'adjoin-term (cons 'term term) term-list))
  (define (first-term term-list) (apply-generic 'first-term term-list))
  (define (rest-terms term-list) (apply-generic 'rest-terms term-list))
  (define (negate-termlist term-list) (apply-generic 'negate-termlist term-list))
  

  ;makes a list, using the length-1 as the order

  (define (first-term_old term-list) (car term-list))
  
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))


  
  (define (tag p) (attach-tag 'polynomial p))
  (define (poly? p) (if (pair? p)
                        (eq? (type-tag p) 'polynomial)
                        #f))

  (define (traverse p)
    (define (travIter p varList orderList)
      (if (empty-termlist? (term-list p)) '()
      (let* ((terms (term-list p))
            (variable (variable p))
            (firstTerm (first-term terms))
            (firstCoeff (coeff firstTerm))
            (restTerms (rest-terms terms)))
        (cond ((=zero? firstCoeff) (travIter (make-poly variable restTerms) varList orderList))
              ((poly? firstCoeff)
               (append (travIter (cdr firstCoeff) (cons variable varList) (cons (order firstTerm) orderList))
                       (travIter (make-poly variable restTerms) varList orderList)))
              (else (append (list (list firstCoeff (cons variable varList) (cons (order firstTerm) orderList)))
                            (travIter (make-poly variable restTerms) varList orderList)))))))
    (travIter p '() '()))

  (define (traverseVars poly)
    (define (consIfNew thing list)
      (if (memq thing list) list
          (cons thing list)))
    (define (copyToList poly results)
      (if (empty-termlist? (term-list poly)) results
      (let* ((terms (term-list poly))
             (variable (variable poly))
             (firstTerm (first-term terms))
             (firstCoeff (coeff firstTerm))
             (restTerms (rest-terms terms)))
        (cond 
              ((poly? firstCoeff)
               (copyToList
                (cdr firstCoeff)
                (consIfNew variable
                      (copyToList
                       (make-poly variable restTerms)
                       results))))
              (else
               (copyToList
                (make-poly variable restTerms)
                (consIfNew variable results)))))))
    (copyToList poly '()))
              

  (define (findDesiredOrder varList orderList desiredVar)
    (cond ((null? varList) 0)
          ((eq? (car varList) desiredVar) (car orderList))
          (else (findDesiredOrder (cdr varList) (cdr orderList) desiredVar))))
  
  (define (allIn list1 list2)
    ;returns whether all values in list1 are in list2
    (cond ((null? list1) #t)
          (else (if (memq (car list1) list2) (allIn (cdr list1) list2) #f))))

  (define (makePolyFromTraverse travList varOrder)
   (define (makePolyIter travList varOrder)
     (if (null? varOrder) (car travList)
         (let* ((varList (cadr travList))
                (orderList (caddr travList))
                (firstVar (car varOrder))
                (desiredOrder (findDesiredOrder varList orderList firstVar)))
           (tag (make-poly firstVar (make-sparse-term-list (make-term desiredOrder (makePolyIter travList (cdr varOrder)))))))))
    (if (allIn (cadr travList) varOrder) (makePolyIter travList varOrder)
        (error "Not all variables in traversal are in the proposed coercion order")))

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op
                        initial
                        (cdr sequence)))))
  (define (makeIdentityPoly varList)
    (if (null? varList) 0
        (tag (make-poly (car varList) (make-sparse-term-list (make-term 0 (makeIdentityPoly (cdr varList))))))))

  (define (reorder p varOrder)
    (let ((identity (makeIdentityPoly varOrder)))
          (accumulate add
                      identity
                      (map (lambda (travList) (makePolyFromTraverse travList varOrder)) (traverse p)))))

  (define (varUnion list1 list2)
    (cond ((null? list1) list2)
          ((memq (car list1) list2) (varUnion (cdr list1) list2))
          (else (varUnion (cdr list1) (cons (car list1) list2)))))
  
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'add-poly-new '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly-new p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'mul-poly-new '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly-new p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub-poly p1 p2))))
  (put '=zero? '(polynomial)
       =zero?-poly)
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (div-poly p1 p2))))
  (put 'traverse '(polynomial)
       traverse)
  (put 'makePolyFromTraverse '(polynomial)
       makePolyFromTraverse)
  (put 'reorder '(polynomial)
       reorder)
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       reduce-poly)
  'done)

(define (add-poly-new p1 p2)
  (apply-generic 'add-poly-new p1 p2))
(define (mul-poly-new p1 p2)
  (apply-generic 'mul-poly-new p1 p2))
(define (traverse p)
  (apply-generic 'traverse p))
(define (reorder p varOrder)
  ((get 'reorder '(polynomial)) (cdr p) varOrder))
(define  (convertTraverse info list)
  ((get 'makePolyFromTraverse '(polynomial)) info list))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (make-dense-term-list . terms)
  ((get 'make-dense-term-list '(dense)) terms))
(define (make-sparse-term-list . terms)
  ((get 'make-sparse-term-list '(sparse)) terms))


(define (add x y) (apply-generic 'add x y))
(define (add3 a b c) (apply-generic 'add3 a b c))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? n) (apply-generic '=zero? n))
(define (raise n) (apply-generic 'raise n))
(define (project n) (apply-generic 'project n))
(define (squareGeneric n) (apply-generic 'square n))
(define (sqrtGeneric n) (apply-generic 'sqrt n))
(define (cosGeneric n) (apply-generic 'cos n))
(define (sinGeneric n) (apply-generic 'sin n))
(define (atanGeneric y x) (apply-generic 'atan y x))
(define (negate n) (apply-generic 'negate n))

(define (install-integer-package)
  (define (tag x)
    (if (integer? x) x (error "Not an integer:" x)))
  (define (=zero? n)
    (= n 0))
  (define (raise n)
    (make-rational n 1))
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (make-real (/ x y))))
  (put 'make 'integer
       (lambda (x) (tag x)))
  ;equality isn't tagged because it's just a boolean value
  (put 'equ? '(integer integer)
       =)
  (put '=zero? '(integer)
       =zero?)
  (put 'add3 '(integer integer integer)
       (lambda (a b c) (tag (+ a b c))))
  (put 'raise '(integer)
       raise)
  (put 'square '(integer)
       (lambda (x) (tag (* x x))))
  (put 'sqrt '(integer)
       (lambda (x)
          (make-real (sqrt x))))
  (put 'atan '(integer integer)
       (lambda (y x)
          (make-real (atan y x))))
  (put 'cos '(integer)
       (lambda (x)
          (make-real (cos x))))
  (put 'sin '(integer)
       (lambda (x)
          (make-real (atan x))))
  (put 'negate '(integer)
       (lambda (x) (tag (* -1 x))))
  (put 'greatest-common-divisor '(integer integer)
       (lambda (a b) (tag (gcd a b))))
  (put 'reduce '(integer integer)
       reduce-integers)
  'done)

(define (install-real-package)
  (define (tag x)
    (if (real? x) (attach-tag 'real x)
        (error "Not a real number: " x)))
  (define (=zero? n)
    (= n 0))
  (define (raise n)
    (make-complex-from-real-imag (make-real n) (make-integer 0)))
  (define (project n)
    (make-rational (round n) 1))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  ;equality isn't tagged because it's just a boolean value
  (put 'equ? '(real real)
       =)
  (put '=zero? '(real)
       =zero?)
  (put 'add3 '(real real real)
       (lambda (a b c) (tag (+ a b c))))
  (put 'raise '(real)
       raise)
  (put 'project '(real)
       project)
  (put 'square '(real)
       (lambda (x) (tag (* x x))))
  (put 'sqrt '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'sqrt '(real)
       (lambda (x)
          (make-real (sqrt x))))
  (put 'atan '(real real)
       (lambda (y x)
          (make-real (atan y x))))
  (put 'cos '(real)
       (lambda (x)
          (make-real (cos x))))
  (put 'sin '(real)
       (lambda (x)
          (make-real (sin x))))
  (put 'negate '(real)
       (lambda (x)
         (make-real (* -1 x))))
  (put 'greatest-common-divisor '(real real)
       (lambda (a b) (tag (gcd a b))))
  'done)


(define (make-real n)
  ((get 'make 'real) n))



(define (make-integer n)
  ((get 'make 'integer) n))

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

(define (reduce a b)
  (apply-generic 'reduce a b))

(define (install-rational-package)
  ;;internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons (car reduced) (cadr reduced))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (add3 a b c)
    (add-rat (add-rat a b) c))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equal-rat x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (=zero?-rat n)
    (=zero? (numer n)))
  (define (raise n)
    (make-real (/ (numer n) (denom n))))
  (define (project n)
    (make-integer (round (/ (numer n)
                            (denom n)))))
  (define (square n)
    (mul-rat n n))
  (define (root n)
    (make-real (sqrt (/ (numer n) (denom n)))))
  (define (atan-rats y x)
    (make-real (atan (/ (numer y) (denom y))
                     (/ (numer x) (denom x)))))
  (define (sin-rat n)
    (make-real (sin (/ (numer n) (denom n)))))
  (define (cos-rat n)
    (make-real (cos (/ (numer n) (denom n)))))

  (define (negate-rat n)
    (make-rat (* (numer n) -1)
              (denom n)))
  ;;interface to rest of system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       equal-rat)
  (put '=zero? '(rational)
       =zero?-rat)
  (put 'add3 '(rational rational rational)
       (lambda (a b c) (tag (add3 a b c))))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'raise '(rational) raise)
  (put 'project '(rational) project)
  (put 'square '(rational)
       (lambda (x) (tag (square x))))
  (put 'sqrt '(rational) root)
  (put 'atan '(rational rational) atan-rats)
  (put 'cos '(rational) cos-rat)
  (put 'sin '(rational) sin-rat)
  (put 'negate '(rational)
       (lambda (x) (tag (negate-rat x))))
  'done)

(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))


(define (install-complex-package)
  ;;import procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;;internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (add3 z1 z2 z3)
    (add-complex
     (add-complex z1 z2)
     z3))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (sqrt-complex z)
    (make-from-mag-ang
     (sqrtGeneric (magnitude z))
     (div (angle z) (make-real 2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
  (define (equ-complex z1 z2)
    (and (equ? (real-part z1) (real-part z2))
         (equ? (imag-part z1) (imag-part z2))))
  (define (=zero?-complex z)
    (=zero? (magnitude z)))
  (define (project z)
    (real-part z))
  (define (square z)
    (mul-complex z z))
  (define (negate-complex z)
    (tag (mul-complex z
                 (make-complex-from-real-imag -1 0))))
  ;;interface to rest of system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       equ-complex)
  (put '=zero? '(complex)
       =zero?-complex)
  (put 'add3 '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add3 z1 z2 z3))))
  (put 'project '(complex)
       project)
  (put 'square '(complex)
       square)
  (put 'sqrt '(complex)
       sqrt-complex)
  (put 'negate '(complex)
       negate-complex)
  'done)


(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (square z) (* z z))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cadr z))
  (define (make-from-real-imag x y) 
    (list x y))
  (define (magnitude z)
    (sqrtGeneric (add (squareGeneric (real-part z))
                      (squareGeneric (imag-part z)))))
  ;have to check if both are zero, we return zero if the magnitude is zero (somewhat arbitrary)
  (define (angle z)
    (if (and (=zero? (imag-part z))
             (=zero? (real-part z)))
        0
        (atanGeneric (imag-part z) (real-part z))))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosGeneric a)) (* r (sinGeneric a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cadr z))
  (define (make-from-mag-ang r a) (list r a))
  (define (real-part z)
    (mul (magnitude z) (cosGeneric (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sinGeneric (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrtGeneric (add (squareGeneric x) (squareGeneric y)))
          (atanGeneric (div y x))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (noFalse list)
  (cond ((null? list) #t)
        ((car list) (noFalse (cdr list)))
        (else #f)))

(define (allEqual list)
  (define (allEqualIter list first)
    (cond ((null? list) #t)
          ((eq? first (car list))
           (allEqualIter (cdr list) first))
          (else #f)))
  (allEqualIter list (car list)))


(define (higher? n1 n2)
  (let ((level1 (levelsFromTop n1))
        (level2 (levelsFromTop n2)))
    (cond ((= level1 level2) (error "Neither is higher! Both same level"))
          ((> level1 level2) #f)
          ((< level1 level2) #t))))

(define (listMin list)
  (define (minIter min list)
    (cond ((null? list) min)
          ((< min (car list)) (minIter min (cdr list)))
          ((= min (car list)) (minIter min (cdr list)))
          ((> min (car list)) (minIter (car list) (cdr list)))))
  (if (null? list) (error "No minimum, list is null!")
      (minIter (car list) (cdr list))))

(define (getRaiseList argList)
  (let ((levelList
         (map levelsFromTop argList)))
    (map (lambda (x) (- x (listMin levelList))) levelList)))

(define  (raiseByList argList raiseList)
  (define (raiseByCount argument count)
    (cond ((= count 0) argument)
          (else (raiseByCount (raise argument) (- count 1)))))
  (define (raiseByListIter argList raiseList resultList)
    (cond ((not (= (length argList)
                  (length raiseList)))
               (error "Argument list and raise list aren't the same length" (length argList)
                      (length raiseList)))
          ;problem: you need to reverse the list to keep the arguments in the same order!
          ((null? argList) (reverse resultList))
          (else (raiseByListIter (cdr argList) (cdr raiseList) (cons
                                                                (raiseByCount (car argList)
                                                                              (car raiseList))
                                                                resultList)))))
  (raiseByListIter argList raiseList '()))





(define (recursiveSearch term list)
  (cond ((null? list) #f)
        ((not (pair? list))
         (if (eq? list term) #t #f))
        ((pair? (car list))
         (or (recursiveSearch term (cdr list))
             (recursiveSearch term (car list))))
        ((eq? term (car list)) #t)
        (else (recursiveSearch term (cdr list)))))

;;implementation of the put/get procedures (treated as an abstraction here)

(define global-array '())
(define coercion-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(install-integer-package)
(install-real-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)
(install-polynomial-package)

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (/* a b)
  (* (/ a b) 1.0))






