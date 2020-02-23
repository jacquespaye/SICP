#lang sicp


;--------------------------------------------------------------
;Section 2.3, Huffman Encoding, Exercise 2.67


(define sampleTree
  (makeCodeTree
   (makeLeaf 'A 4)
   (makeCodeTree
    (makeLeaf 'B 2)
    (makeCodeTree
     (makeLeaf 'D 1)
     (makeLeaf 'C 1)))))

(define sampleMessage
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;result of sample message--"ADABBCA"

;--------------------------------------------------------------
;Section 2.3, Huffman Encoding, Exercise 2.68


(define (encodeSymbol symbol tree)
  (cond ((not (contained? symbol (symbols tree))) (error "Cannot find requested symbol in tree:" symbol))
        ((leaf? tree) '())
        ((contained? symbol (symbols (leftBranch tree)))
         (cons 0 (encodeSymbol symbol (leftBranch tree))))
        ((contained? symbol (symbols (rightBranch tree)))
         (cons 1 (encodeSymbol symbol (rightBranch tree))))))


;--------------------------------------------------------------
;Section 2.3, Huffman Encoding, Exercise 2.69
(define (generateHuffmanTree pairs)
  (successiveMerge
   (makeLeafSet pairs)))

(define (successiveMerge pairList)
  (cond ((null? pairList) '())
        ((null? (cdr pairList)) (car pairList))
        (else (successiveMerge (adjoinSet
                                (makeCodeTree (car pairList)
                                              (cadr pairList))
                                (cddr pairList))))))

;--------------------------------------------------------------
;Section 2.3, Huffman Encoding, Exercise 2.70
(define rockLyrics (generateHuffmanTree (list '(A 2) '(BOOM 1) '(GET 2) '(JOB 2) '(NA 16) '(SHA 3) '(YIP 9) '(WAH 1))))

(define lyrics
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP  YIP YIP YIP YIP YIP SHA BOOM))



;--------------------------------------------------------------
;Utility functions for this section:

(define (makeLeaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbolLeaf x) (cadr x))
(define (weightLeaf x) (caddr x))

(define (makeCodeTree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (leftBranch tree)
  (car tree))

(define (rightBranch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbolLeaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weightLeaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode1 bits currentBranch)
    (if (null? bits)
        '()
        (let ((nextBranch
               (chooseBranch
                (car bits)
                currentBranch)))
          (if (leaf? nextBranch)
              (cons
               (symbolLeaf nextBranch)
               (decode1 (cdr bits) tree))
              (decode1 (cdr bits)
                       nextBranch)))))
  (decode1 bits tree))

(define (chooseBranch bit branch)
  (cond ((= bit 0) (leftBranch branch))
        ((= bit 1) (rightBranch branch))
        (else (error "bad bit: chooseBranch" bit))))

;this inserts an element x in the proper position to keep things ordered by weight
(define (adjoinSet x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoinSet x (cdr set))))))


;adjoins the leaves repeatedly to produce an ordered list
(define (makeLeafSet pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoinSet
         (makeLeaf (car pair)
                   (cadr pair))
         (makeLeafSet (cdr pairs))))))




(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encodeSymbol (car message)
                     tree)
       (encode (cdr message) tree))))

(define (contained? symbol list)
  (cond ((null? list) false)
        ((eq? symbol (car list)) true)
        (else (contained? symbol (cdr list)))))







        