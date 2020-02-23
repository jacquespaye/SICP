#lang scheme

;--------------------------------------------------------------
;Section 2.1, Exercise 2.17
(define (lastPair l)
  (if (null? (cdr l)) l
      (lastPair (cdr l))))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.18
(define (reverseRec l)
  (if (null? l) l
  (append (reverseRec (cdr l)) (list (car l)))))

(define (reverse l)
  (define (revIter l r)
    (if (null? l) r
        (revIter (cdr l) (cons (car l) r))))
  (revIter l '()))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.19

(define (cc amount coinValues)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (noMore coinValues)) 
         0)
        (else
         (+ (cc 
             amount
             (exceptFirstDenomination 
              coinValues))
            (cc 
             (- amount
                (firstDenomination 
                 coinValues))
             coinValues)))))

(define (noMore coins)
  (null? coins))

(define (firstDenomination coins)
  (car coins))

(define (exceptFirstDenomination coins)
  (cdr coins))

(define usCoins (list 50 25 10 5 1))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.20

(define (sameParity . items)
  (let ((test (if (even? (car items)) even? odd?)))
    (define (iterator items results)
      (cond ((null? items) results)
            ((test (car items)) (iterator (cdr items) (cons (car items) results)))
            (else (iterator (cdr items) results))))
    (iterator (reverse items) '())))

(define (sameParity2 . items)
  (let ((first (car items)))
    (define (iterator items results)
      (cond ((null? items) results)
            ((even? (- (car items) first)) (iterator (cdr items) (cons (car items) results)))
            (else (iterator (cdr items) results))))
  (iterator (reverse items) '())))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.21
(define (scaleList list factor)
  (map (lambda (x) (* x factor)) list))

(define (squareList1 l)
  (if (null? l) '()
      (cons (square (car l)) (squareList1 (cdr l)))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.22

;produces a reverse order list because we take the first term of the list, and put it first into the results list (making it deepest in the cons chain)
;inverting the arguments doesn't work either because it doesn't create the right list formatting, putting the cons chain first and the number second.



;--------------------------------------------------------------
;Section 2.1, Exercise 2.23


(define (forEach f items)
  (if (null? items) #t
      ((lambda (x) (f (car x)) (forEach f (cdr x))) items)))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.25

(define s (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr s))))))))))))




;--------------------------------------------------------------
;Section 2.1, Exercise 2.27

(define (deepReverse l)
  (define (deepRevIter l a)
    (cond ((null? l) a)
          ((not (pair? l)) l)
          (else (deepRevIter (cdr l)
                             (cons (deepRevIter (car l) '()) a)))))
  (deepRevIter l '()))



;--------------------------------------------------------------
;Section 2.1, Exercise 2.28

(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? l)) (list l))
        (else (append (fringe (car l)) (fringe (cdr l))))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.29
(define (leftBranch m)
  (car m))

(define (makeMobile left right)
  (cons left right))

(define (rightBranch m)
  (cdr m))

(define (totalWeight m)
  (if (not (pair? m)) m
      (+ (totalWeight (branchStructure (leftBranch m)))
         (totalWeight (branchStructure (rightBranch m))))))

(define (branchBalanced? b)
  (if (pair? (branchStructure b))
      (balanced? (branchStructure b))
      #t))

(define (balanced? m)
  (and (= (branchTorque (rightBranch m))
          (branchTorque (leftBranch m)))
       (branchBalanced? (leftBranch m))
       (branchBalanced? (rightBranch m))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.30
(define (squareTree tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (squareTree (car tree)) (squareTree (cdr tree))))
        (else (square tree))))

(define (squareTreeMap tree)
  (map (lambda (subTree)
         (if (pair? subTree)
             (squareTreeMap subTree)
             (square subTree)))
       tree))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.31

(define (treeMap f tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (treeMap f (car tree))
                            (treeMap f (cdr tree))))
        (else (f tree))))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
; only two options--either contains the first element or not. If not, subset of the second two elements. If yes, just add the first element
;to the subsets of the right


;--------------------------------------------------------------
;Section 2.1, Exercise 2.33


;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.34

(define (hornerEval x coeffSeq)
  (accumulate
   (lambda (thisCoeff higherTerms)
     (+ thisCoeff (* x higherTerms)))
   0
   coeffSeq))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.35

(define (countLeaves x)
  (accumulate + 0
              (map
               (lambda (x) (if (pair? x) (countLeaves x) 1))
               x)))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.36
(define (accumulateN op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulateN op init (map (lambda (x) (cdr x)) seqs)))))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.37
(define (dotProduct v w)
  (accumulate + 0 (map * v w)))

(define (matrixVector m v)
  (map (lambda (row) (dotProduct v row)) m))

(define q (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (transpose m)
  (accumulateN cons '() m))

(define (matrixMatrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrixVector cols x)) m)))



(define (foldRight op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (foldRight op 
                     initial 
                     (cdr sequence)))))

(define (foldLeft op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.38

;prediction for (fold-right / 1 (list 1 2 3))  -- (1/(2/(3/1))=1/(2/3)=3/2
;prediction for (fold-left  / 1 (list 1 2 3)) -- (((1/1)/2)/3)=(1/2)/3=1/6
;prediction for (fold-right list nil (list 1 2 3)) -- (1 (2 (3 '())))
;prediction for (fold-left  list nil (list 1 2 3)) -- ((('() 1) 2) 3)

;Property that must be satisfied--associative property -- aka (op (op a b) c)=(op a (op b c))
;also commutativity, because the location of the initial value varies depending on whether we fold right or left


;--------------------------------------------------------------
;Section 2.1, Exercise 2.39


(define (reverseRight sequence)
  (foldRight
   (lambda (x y) (append y (list x))) '() sequence))

(define (reverseLeft sequence)
  (foldLeft (lambda (x y) (cons y x) ) '() sequence))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.40


(define (makePairs n) (accumulate
                       append
                       '()
                       (map (lambda (i)
                              (map (lambda (j)
                                     (list i j))
                                   (enumerateInterval 1 (- i 1))))
                            (enumerateInterval 1 n))))

(define (makePairs2 n)
  (flatMap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerateInterval 1 (- i 1))))
           (enumerateInterval 1 n)))

(define (makePairSum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (primeSumPairs n)
  (map makePairSum
       (filter
        primeSum? (makePairs2 n))))

;--------------------------------------------------------------
;Section 2.1, Exercise 2.41
(define (generateTriples n)
  (flatMap (lambda (i)
             (map (lambda (j) (append i (list j)))
                  (enumerateInterval 1 (- (cadr i) 1))))
           (flatMap (lambda (i)
                      (map (lambda (j) (list i j)) (enumerateInterval 2 (- i 1))))
                    (enumerateInterval 3 n))))


(define (tripleSum n s)
  (filter (lambda (i)
            (= (accumulate + 0 i)
               s)) (generateTriples n)))

(define (tripleSum? triple sum)
  (= (accumulate + 0 triple) sum))

(define (createTripleSum triple)
  (append triple (list (accumulate + 0 triple))))

(define (findTripleSums n s)
  (map createTripleSum (filter (lambda (x) (tripleSum? x s)) (generateTriples n))))



;--------------------------------------------------------------
;Section 2.1, Exercise 2.42

(define (makeQueen row col) (cons row col))
(define (queenCol queen) (cdr queen))
(define (queenRow queen) (car queen))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (makeQueen new-row k) rest-of-queens))

(define (safe? k positions)
  (cond ((not (null? (filter (lambda (queen) (= (queenCol queen) (queenCol (car positions)))) (cdr positions)))) #f)
        ((not (null? (filter (lambda (queen) (= (queenRow queen) (queenRow (car positions)))) (cdr positions)))) #f)
        ((not (null? (filter (lambda (queen) (= (abs (- (queenCol queen)
                                                        (queenCol (car positions))))
                                                (abs (- (queenRow queen)
                                                        (queenRow (car positions)))))) (cdr positions)))) #f)
        (else #t)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatMap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerateInterval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


;--------------------------------------------------------------
;Section 2.1, Exercise 2.43

(define (queensSlow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatMap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position 
                    new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerateInterval 1 board-size)))))
  (queen-cols board-size))

;need flatmap because each list creates a list of lists

;runs slowly because it now runs queens-cols once per column instead of just once per level


;--------------------------------------------------------------
;Utility functions for this section:

(define (listRef l n)
  (if (= n 0) (car l)
      (listRef (cdr l) (- n 1))))

(define (len l)
  (if (null? l) 0
  (+ 1 (len (cdr l)))))

(define (lenIter l)
  (define (countIter l c)
    (if (null? l) c
        (countIter (cdr l) (+ c 1))))
  (countIter l 0))

(define rex (list 1 2 3 4 5 6 7))
(define regis (list 11 22 33))

(define (append a b)
  (if (null? a) b
      (cons (car a) (append (cdr a) b))))




(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (= (remainder x 2) 1))


(define (map f list)
  (if (null? list) '()
      (cons (f (car list)) (map f (cdr list)))))

(define (square x) (* x x))

(define (squareList2 l)
  (map square l))

(define (squareListIter items)
  (define (iter items answer)
    (if (null? items) answer
        (iter (cdr items)
              (cons (square (car items))
                    answer))))
  (iter items '()))

(define (lengthList l)
  (if (null? l) 0 (+ 1 (lengthList (cdr l)))))

(define (countLeaves2 t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (countLeaves2 (car t)) (countLeaves2 (cdr t))))))


(define (append a b)
  (if (null? a) b
      (cons (car a) (append (cdr a) b))))

(define (reverse l)
  (define (reverseIter l a)
    (if (null? l) a
    (reverseIter (cdr l) (cons (car l) a))))
  (reverseIter l '()))




(define (makeBranch length structure)
  (cons length structure))

(define (branchLength b)
  (car b))

(define (branchStructure b)
  (cdr b))

(define (sumList l)
  (if (null? l) 0
      (+ (car l) (sumList (cdr l)))))



(define (branchTorque b)
  (* (totalWeight (branchStructure b)) (branchLength b)))



(define (scaleTree t f)
  (cond ((null? t) '())
        ((not (pair? t)) (* t f))
        (else (cons (scaleTree (car t) f)
                    (scaleTree (cdr t) f)))))

(define (scaleTreeMap tree factor)
  (map (lambda (subTree)
         (if (pair? subTree)
             (scaleTreeMap subTree factor)
             (* subTree factor)))
       tree))

(define (square x) (* x x))




(define (sumOddSquares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) (if (odd? tree) (square tree) 0))
          (else (+ (sumOddSquares (car tree))
                   (sumOddSquares (cdr tree))))))

(define (evenFibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerateInterval low high)
  (if (> low high) '()
      (cons low (enumerateInterval (+ low 1) high))))

(define (enumerateTree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerateTree (car tree)) (enumerateTree (cdr tree))))))

(define (sumOddSquaresSignal tree)
  (accumulate + 0
              (map square
                   (filter odd?
                           (enumerateTree tree)))))

(define (evenFibsSignal n)
  (accumulate cons '()
              (filter even?
                      (map fib
                           (enumerateInterval 0 n)))))

(define (listFibSquares n)
  (accumulate cons '() (map square (map fib (enumerateInterval 0 n)))))

(define (productOfSquaresOfOdd list)
  (accumulate * 1 (map square (filter odd? list))))






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

(define (square x) (* x x))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))




(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))





(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (flatMap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerateInterval low high)
  (if (> low high) '()
      (cons low (enumerateInterval (+ low 1) high))))



(define (smallest-divisor n)
  (find-divisor n 2))

(define (primeSum? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s) (list '())
      (flatMap (lambda (i)
                 (map (lambda (z) (cons i z)) (permutations (remove i s)))) s)))







