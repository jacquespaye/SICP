#lang sicp

;--------------------------------------------------------------
;Section 2.3, Exercise 2.53

;(list 'a 'b 'c)
; (a b c)

;(list (list 'george))
;((george))

;(cdr '((x1 x2) (y1 y2)))
;((y1 y2))

;(cadr '((x1 x2) (y1 y2)))
;(y1 y2)


;(pair? (car '(a short list)))
;(pair? a), #f

;(memq 'red '((red shoes) (blue socks)))
;#f

;(memq 'red '(red shoes blue socks))
;(red shoes blue socks)


;--------------------------------------------------------------
;Section 2.3, Exercise 2.54

(define (equal? a b)
  (cond ((eq? a b) #t)
        ((and (pair? a)
              (pair? b)) (and (equal? (car a)
                                      (car b))
                              (equal? (cdr a)
                                      (cdr b))))
        (else #f)))


;--------------------------------------------------------------
;Section 2.3, Exercise 2.55

;(car ''abracadabra)
;this evaluates as (car (quote (quote abracadabra)))
;evaluates to (car (quote abracadabra))
;thus (car (quote abracadabra))= quote
;and (cdr (quote abracadabra))=(abracadabra)




;--------------------------------------------------------------
;Section 2.3, Exercise 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (sameVariable exp var) 1 0))
        ((sum? exp)
         (makeSum2 (deriv (addend2 exp) var)
                  (deriv (augend2 exp) var)))
        ((product? exp)
         (makeSum2
          (makeProduct2
           (multiplier2 exp)
           (deriv (multiplicand2 exp) var))
          (makeProduct2
           (deriv (multiplier2 exp) var)
           (multiplicand2 exp))))
        ((power? exp)
         (makeProduct2 (makeProduct2
                        (exponent exp)
                        (makePower (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

;--------------------------------------------------------------
;Section 2.3, Exercise 2.57


;below the implementation for more than 2 arguments in sums
(define (makeSum2 first rest)
  (cond ((=number? first 0) rest)
        ((=number? rest 0) first)
        ((null? rest) first)
        ((and (number? first) (number? rest))
         (+ first rest))
        (else (list '+ first rest))))

(define (addend2 sum)
  (cadr sum))

(define (augend2 sum)
  (cond ((null? (cdddr sum)) (caddr sum))
        (else (cons '+ (cddr sum)))))




(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (variable? x) (symbol? x))

(define (sameVariable v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (makeSum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
;product implementation for multiple args

(define (makeProduct2 first rest)
  (cond ((or (=number? first 0)
             (=number? rest 0))
         0)
        ((=number? first 1) rest)
        ((=number? rest 1) first)
        ((and (number? first)
              (number? rest))
         (* first rest))
        (else (list '* first rest))))

(define (multiplier2 product)
  (cadr product))

(define (multiplicand2 product)
  (cond ((null? (cdddr product)) (caddr product))
        (else (cons '* (cddr product)))))


;--------------------------------------------------------------
;Section 2.3, Exercise 2.58

(define (derivInfix exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (sameVariable exp var) 1 0))
        ((sumInfix? exp)
         (makeSumInfix (derivInfix (addendInfix exp) var)
                       (derivInfix (augendInfix exp) var)))
        ((productInfix? exp)
         (makeSumInfix
          (makeProductInfix
           (multiplierInfix exp)
           (derivInfix (multiplicandInfix exp) var))
          (makeProductInfix
           (derivInfix (multiplierInfix exp) var)
           (multiplicandInfix exp))))
        ((powerInfix? exp)
         (makeProductInfix (makeProductInfix
                            (exponentInfix exp)
                            (makePowerInfix (baseInfix exp) (- (exponentInfix exp) 1)))
                           (derivInfix (baseInfix exp) var)))
        (else (error "unknown expression type: DERIV" exp))))


;--------------------------------------------------------------
;Section 2.3, Exercise 2.59

(define (unionSet set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        ((elementOfSet? (car set1) set2)
         (unionSet (cdr set1) set2))
        (else (cons (car set1)
                    (unionSet (cdr set1) set2)))))

;--------------------------------------------------------------
;Section 2.3, Exercise 2.60

(define (adjoinSetDup x set)
  (cons x set))

(define (unionSetDups set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        (else (append set1 set2))))

;Assuming we allow duplicates--elementofset is the same
;adjoinset--faster because we don't need to check if it's already there O(1)
;intersectionset--slower because we are checking longer lists O(n^2)
;unionset--faster because we don't  have to check if it's in 2 O(N)
;element? O(n)

;I would use the duplicate process if I was adjoining a lot O(N) or using a ton of unions O(1), and using intersections rarely O(n^2)
;I would use the non-duplicate process if I was using intersections often and adjoining less
;Allowing duplicates significantly increases the memory overhead of the program



;--------------------------------------------------------------
;Section 2.3, Exercise 2.61

(define (adjoinSet x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoinSet x (cdr set))))))

;--------------------------------------------------------------
;Section 2.3, Exercise 2.62

(define (unionSet set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2) (cons x1 (unionSet (cdr set1) set2)))
                      ((< x2 x1) (cons x2 (unionSet set1 (cdr set2))))
                      ((= x1 x2) (cons x1 (unionSet (cdr set1) (cdr set2)))))))))



;--------------------------------------------------------------
;Section 2.3, Exercise 2.63

;While the methods differ  (adding to a "results" list or consing from the middle out), these procedures do produce the same result for any tree
;The methods always produce (1 3 5 7 9 11)

;No, these do not have the same order of growth. The "append" procedure is quite costly, and method 1 requires appending half of the tree at every level.
;Whereas method 2 allows us to put a lot of the tree into a list and not have to append it to anything
;When we put something new in and use method1, it now has to be appended many times, whereas it just has to be put into a list once
;O(n) vs. O(nlogn)


;--------------------------------------------------------------
;Section 2.3, Exercise 2.65

(define (unionTree t1 t2)
  (listTree
   (unionSet (treeList t1)
             (treeList t2))))

(define (intersectionTree t1 t2)
  (listTree
   (intersectionSet (treeList t1)
                    (treeList t2))))


;--------------------------------------------------------------
;Section 2.3, Exercise 2.66

(define (lookup givenKey setOfRecords)
  (cond ((null? setOfRecords) false)
        ((equal? givenKey (key (car setOfRecords)))
         (car setOfRecords))
        (else
         (lookup givenKey (cdr setOfRecords)))))

(define (lookupTree givenKey treeOfRecords)
  (cond ((null? treeOfRecords) false)
        ((= givenKey (key (entry treeOfRecords))) (entry treeOfRecords))
        ((< givenKey (key (entry treeOfRecords))) (lookupTree givenKey (left-branch treeOfRecords)))
        ((> givenKey (key (entry treeOfRecords))) (lookupTree givenKey (right-branch treeOfRecords)))))





;--------------------------------------------------------------
;Utility functions for this section:


(define (treeList tree)
  (define (copyToList tree resultList)
    (if (null? tree)
        resultList
        (copyToList
         (leftBranch tree)
         (cons (entry tree)
               (copyToList
                (rightBranch tree)
                resultList)))))
  (copyToList tree '()))

(define (entry tree) (car tree))
(define (leftBranch tree) (cadr tree))
(define (rightBranch tree) (caddr tree))
(define (makeTree entry left right)
  (list entry left right))


(define (listTree elements)
  (car (partialTree
        elements (length elements))))

(define (partialTree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((leftSize
             (quotient (- n 1) 2)))
        (let ((leftResult
               (partialTree
                elts leftSize)))
          (let ((leftTree
                 (car leftResult))
                (nonLeftElts
                 (cdr leftResult))
                (rightSize
                 (- n (+ leftSize 1))))
            (let ((thisEntry
                  (car nonLeftElts))
                  (rightResult
                   (partialTree
                    (cdr nonLeftElts)
                    rightSize)))
              (let ((rightTree
                     (car rightResult))
                    (remainingElts
                     (cdr rightResult)))
                (cons (makeTree thisEntry
                                leftTree
                                rightTree)
                      remainingElts))))))))

(define (makeList a b starter)
  (if (= a b) (reverse starter)
      (makeList (+ a 1) b (cons a starter))))

(define (unionSet set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2) (cons x1 (unionSet (cdr set1) set2)))
                      ((< x2 x1) (cons x2 (unionSet set1 (cdr set2))))
                      ((= x1 x2) (cons x1 (unionSet (cdr set1) (cdr set2)))))))))

(define (intersectionSet set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersectionSet (cdr set1)
                                         (cdr set2))))
               ((< x1 x2) (intersectionSet
                           (cdr set1) set2))
               ((< x2 x1) (intersectionSet
                           set1 (cdr set2)))))))






;----------------------------------------------------------
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;-------------------------------------------------------------

(define (makeProduct m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier m)
  (cadr m))

(define (multiplicand m)
  (caddr m))

(define  (power? x)
  (and (pair? x) (eq? (car x) '**)))

(define (makePower base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (base power)
  (cadr power))

(define (exponent power)
  (caddr power))

;----------------------------------------------------------
(define (makePowerInfix base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list base '^ exponent))))

(define (baseInfix power)
  (car power))

(define (exponentInfix power)
  (caddr power))


(define (powerInfix? x)
  (and (pair? x) (eq? (cadr x) '^)))

;------------------------------------------------
(define (makeSumInfix a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addendInfix sum)
  (car sum))

(define (augendInfix sum)
  (caddr sum))

(define (sumInfix? x)
  (and (pair? x) (eq? (cadr x) '+)))
;---------------------------------------------------
(define (makeProductInfix m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (multiplierInfix m)
  (car m))

(define (multiplicandInfix m)
  (caddr m))

(define (productInfix? x)
  (and (pair? x) (eq? (cadr x) '*)))

;---------------------------------------------------

(define (elementOfSet? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (elementOfSet? x (cdr set)))))

(define (adjoinSet x set)
  (if (elementOfSet? x set) set
      (cons x set)))

(define (intersectionSet set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((elementOfSet? (car set1) set2)
         (cons (car set1)
               (intersectionSet (cdr set1) set2)))
        (else (intersectionSet (cdr set1) set2))))


(define (append1 l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append1 (cdr l1) l2))))


  
(define (entry tree) (car tree))
(define (leftBranch tree) (cadr tree))
(define (rightBranch tree) (caddr tree))
(define (makeTree entry left right)
  (list entry left right))


(define (elementOfSet? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (elementOfSet?
          x
          (leftBranch set)))
        ((> x (entry set))
         (elementOfSet?
          x
          (rightBranch set)))))

(define (adjoinSet x set)
  (cond ((null? set) (makeTree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (makeTree (entry set)
                   (adjoinSet x (leftBranch set))
                   (rightBranch set)))
        ((> x (entry set))
         (makeTree (entry set)
                   (leftBranch set)
                   (adjoinSet x (rightBranch set))))))

(define (treeList1 tree)
  (if (null? tree)
      '()
      (append
       (treeList1
        (leftBranch tree))
       (cons (entry tree)
             (treeList1
              (rightBranch tree))))))

(define (treeList2 tree)
  (define (copyToList tree resultList)
    (if (null? tree)
        resultList
        (copyToList
         (leftBranch tree)
         (cons (entry tree)
               (copyToList
                (rightBranch tree)
                resultList)))))
  (copyToList tree '()))

(define t1
  (makeTree 7 (makeTree 3 (makeTree 1 '() '()) (makeTree 5 '() '())) (makeTree 9 '() (makeTree 11 '() '()))))

(define t2
  (makeTree 3 (makeTree 1 '()'()) (makeTree 7 (makeTree 5 '() '()) (makeTree 9 '() (makeTree 11 '() '())))))

(define t3
  (makeTree 5 (makeTree 3 (makeTree 1 '() '()) '()) (makeTree 9 (makeTree 7 '() '()) (makeTree 11 '() '()))))




