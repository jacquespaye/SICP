#lang sicp
(#%require "Section-4.3--AmbiguousEvaluator")
(#%require schemeunit)




;--------------------------------------------------------------
;Section 4.3, Exercise 4.35
;;I assume that the interval is defined [a,b)
(interpret '(define (an-integer-between a b)
              (require (<= a b))
              (amb a (an-integer-between (+ a 1) b))))



;--------------------------------------------------------------
;Section 4.3, Exercise 4.36
(interpret '(define (all-pythagorean-triples start)
              (define (pyth-below high-num)
                (let ((k high-num))
                  (let ((j (an-integer-between 1 k)))
                    (let ((i (an-integer-between 1 j)))
                      (require (= (+ (* i i) (* j j))
                                  (* k k)))
                      (list i j k)))))
              (define (pyth-iter num)
                (amb (pyth-below num) (pyth-iter (+ num 1))))
              (pyth-iter start)))


;--------------------------------------------------------------
;Section 4.3, Exercise 4.38


(interpret '(define (multiple-dwelling)
              (let ((baker (amb 1 2 3 4 5))
                    (cooper (amb 1 2 3 4 5))
                    (fletcher (amb 1 2 3 4 5))
                    (miller (amb 1 2 3 4 5))
                    (smith (amb 1 2 3 4 5)))
                (require (distinct? (list baker cooper fletcher miller smith)))
                (require (not (= baker 5)))
                (require (not (= cooper 1)))
                (require (not (= fletcher 5)))
                (require (not (= fletcher 1)))
                (require (> miller cooper))
                (require (not (= (abs (- smith fletcher)) 1)))
                (require (not (= (abs (- fletcher cooper)) 1)))
                (list (list 'baker baker)
                      (list 'cooper cooper)
                      (list 'fletcher fletcher)
                      (list 'miller miller)
                      (list  'smith smith))
                )))


;--------------------------------------------------------------
;Section 4.3, Exercise 4.40

(interpret '(define (multiple-dwelling-2)
              (let ((cooper (an-element-of-except '(2 3 4 5) '())))
                (let ((miller (an-element-of-except '(1 2 3 4 5) (list cooper))))
                  (require (> miller cooper)) ;moving this to the end approximately doubles the search time            
                  (let ((fletcher (an-element-of-except '(2 3 4) (list miller cooper))))
                    (require (not (= (abs (- fletcher cooper)) 1)))
                    (let ((smith  (an-element-of-except '(1 2 3 4 5) (list miller cooper fletcher))))
                      (require (not (= (abs (- smith fletcher)) 1)))
                      (let ((baker (an-element-of-except '(1 2 3 4) (list miller cooper fletcher smith))))
                        (list (list 'baker baker)
                              (list 'cooper cooper)
                              (list 'fletcher fletcher)
                              (list 'miller miller)
                              (list  'smith smith))
                        )))))))

;--------------------------------------------------------------
;Section 4.3, Exercise 4.41

(define (multiple-scheme)
  (define (make-assignment names floors)
    (list names floors))
  (define (assignment-names assignment) (car assignment))
  (define (assignment-floors assignment) (cadr assignment))

  (define (make-parameters name options conditions)
    (list name options conditions))
  (define (parameter-name parameter) (car parameter))
  (define (parameter-options parameter) (cadr parameter))
  (define (parameter-conditions parameter) (caddr parameter))

  (define (get-floor name assignment)
    (let ((names (assignment-names assignment))
          (floors (assignment-floors assignment)))
      (define (iter names floors)
        (cond ((null? names) assignment)
              ((eq? (car names) name) (car floors))
              (else (iter (cdr names) (cdr floors)))))
      (iter names floors)))


  
  (define (all-satisfied? restrictions argument)
    (cond ((null? restrictions) #t)
          (((car restrictions) argument) (all-satisfied? (cdr restrictions) argument))
          (else #f)))

  
  (define (append-if assignment name options conditions)
    (define (append-iter options results)
      (cond ((null? options) results)
            (else (let ((proposed (make-assignment
                                   (cons name (assignment-names assignment))
                                   (cons (car options) (assignment-floors assignment)))))
                    (if (all-satisfied? conditions proposed)
                        (append-iter (cdr options) (cons proposed results))
                        (append-iter (cdr options) results))))))
    (append-iter options '()))

  (define (flat-map function list)
   (if (null? (cdr list)) (function (car list))
   (append (function (car list)) (flat-map function  (cdr list)))))

  (define (solve-iter assignments parameters)
    (cond ((null? parameters) assignments)
          ((null? assignments)
           (solve-iter
            (map (lambda (option)
                   (make-assignment
                    (list (parameter-name (car parameters)))
                    (list option)))
                 (parameter-options (car parameters)))
            (cdr parameters)))
          (else    
           (solve-iter
            (flat-map
             (lambda (assignment)
               (append-if
                assignment
                (parameter-name (car parameters))
                (parameter-options (car parameters))
                (parameter-conditions (car parameters))))
             assignments)
               
            (cdr parameters)))))

  (define (book-order assignment)
    (list
     (list 'baker (get-floor 'baker assignment))
     (list 'cooper (get-floor 'cooper assignment))
     (list 'fletcher (get-floor 'fletcher assignment))
     (list 'miller (get-floor 'miller assignment))
     (list 'smith (get-floor 'smith assignment))))
  (let
      ((cooper (make-parameters 'cooper '(2 3 4 5) '()))
       (miller (make-parameters 'miller '(1 2 3 4 5) (list (lambda (assignment)
                                                             (< (get-floor 'cooper assignment)
                                                                (get-floor 'miller assignment)))
                                                           (lambda (assignment)
                                                             (not (memq (car (assignment-floors assignment))
                                                                        (cdr (assignment-floors assignment))))))))
       (fletcher (make-parameters 'fletcher '(2 3 4) (list (lambda (assignment)
                                                                 (not (= (abs (- (get-floor 'fletcher assignment)
                                                                            (get-floor 'cooper assignment))) 1)))
                                                           (lambda (assignment)
                                                             (not (memq (car (assignment-floors assignment))
                                                                        (cdr (assignment-floors assignment))))))))
       (smith (make-parameters 'smith '(1 2 3 4 5) (list (lambda (assignment)
                                                           (not (= (abs (- (get-floor 'smith assignment)
                                                                           (get-floor 'fletcher assignment))) 1)))
                                                         (lambda (assignment)
                                                             (not (memq (car (assignment-floors assignment))
                                                                        (cdr (assignment-floors assignment))))))))
       (baker (make-parameters 'baker '(1 2 3 4) (list (lambda (assignment)
                                                             (not (memq (car (assignment-floors assignment))
                                                                        (cdr (assignment-floors assignment)))))))))
    
    (map (lambda (assignment) (book-order assignment)) (solve-iter '() (list cooper miller fletcher smith baker)))))



;--------------------------------------------------------------
;Section 4.3, Exercise 4.42


(interpret '(define (exam-result)
              (define (make-assignment name place) (cons name place))
              (define (name assign) (car assign))
              (define (place assign) (cdr assign))
              (define (list-names assertions) (map name assertions))
              (define (list-places assertions) (map place assertions))
              (define (fetch-where condition items)
                (define (iter items result)
                  (cond ((null? items) result)
                        ((condition (car items)) (iter (cdr items) (cons (car items) result)))
                        (else (iter (cdr items) result))))
                (iter items '()))
              (define (all-equal? items item)
                (define (iter item-list)
                  (cond ((null? item-list) 'true)
                        ((null? (cdr item-list)) 'true)
                        ((equal? (car item-list) (car (cdr item-list))) (iter (cdr item-list)))
                        (else 'false)))
                (iter (cons item items)))
              (define (consistent? lie-truth lies truths)
                (let ((lie (car lie-truth))
                      (truth (cdr lie-truth)))
                  (require (null? (fetch-where (lambda (x) (equal? x truth)) lies)))
                  (require (null? (fetch-where (lambda (x) (equal? x lie)) truths)))
                  (let ((same-places (fetch-where (lambda (x) (equal? (place x) (place truth))) truths))
                        (same-names (fetch-where (lambda (x) (equal? (name x) (name truth))) truths)))
                    (if (not (null? same-places))
                        (require (all-equal? same-places truth)))
                    (if (not (null? same-names))
                        (require (all-equal? same-names truth))))))
              (define (solve-iter lie-truth-pairs)
                (define (iter lies truths pairs)
                  (if (null? pairs) truths
                      (let ((pair1 (car pairs)))
                       
                        (let ((lie-truth (amb (cons (car pair1)
                                                    (cdr pair1))
                                              (cons (cdr pair1)
                                                    (car pair1)))))
                          (consistent? lie-truth lies truths)
                          (iter (cons (car lie-truth) lies) (cons (cdr lie-truth) truths) (cdr pairs))))))
                (iter '() '() lie-truth-pairs))
              (solve-iter (list
                           (cons (make-assignment 'kitty 2) (make-assignment 'betty 3))
                           (cons (make-assignment 'ethel 1) (make-assignment 'joan 2))
                           (cons (make-assignment 'joan 3) (make-assignment 'ethel 5))
                           (cons (make-assignment 'kitty 2) (make-assignment 'mary 4))
                           (cons (make-assignment 'mary 4) (make-assignment 'betty 1))))))


;--------------------------------------------------------------
;Section 4.3, Exercise 4.43


(interpret '(define (yacht)
              (define (make-assignments)
                (let ((fathers '())
                      (ships '())
                      (daughters '()))
                  (define (put-proc father ship daughter)
                    (require (not (member daughter daughters)))
                    (set! fathers (cons father fathers))
                    (set! ships (cons ship ships))
                    (set! daughters (cons daughter daughters)))
                  (define (list-map id)
                      (cond ((eq? id 'father) fathers)
                            ((eq? id 'ship) ships)
                            ((eq? id 'daughter) daughters)
                            (else (error "Invalid mapping id:" id))))
                  (define (fetch-proc return-id match-id criterion)
                    (define (search-iter list-to-return list-to-match criterion)
                      (cond ((null? list-to-match) 'false)
                            ((equal? (car list-to-match) criterion) (car list-to-return))
                            (else (search-iter (cdr list-to-return) (cdr list-to-match) criterion))))
                    (search-iter (list-map return-id) (list-map match-id) criterion))
                  (lambda (m)
                    (cond ((eq? m 'fetch) fetch-proc)
                          ((eq? m 'put) put-proc)
                          ((eq? m 'list) list-map)
                          (else "Invalid assignment object method:" m)))))
              (let ((assigns (make-assignments)))
                ((assigns 'put) 'barnacle 'gabrielle 'melissa)
                ((assigns 'put) 'moore 'lorna (amb 'rosalind 'gabrielle 'maryann))
                ((assigns 'put) 'parker 'maryann (amb 'rosalind 'gabrielle 'lorna))
                ((assigns 'put) 'downing 'melissa (amb 'rosalind 'gabrielle 'lorna 'maryann))
                ((assigns 'put) 'hall 'rosalind (amb 'gabrielle 'lorna 'maryann))
                (require (equal? ((assigns 'fetch)
                                  'ship
                                  'father
                                  ((assigns 'fetch) 'father 'daughter 'gabrielle))
                                 ((assigns 'fetch) 'daughter 'father 'parker)))
                
                (list (list 'barnacle ((assigns 'fetch) 'daughter 'father 'barnacle))
                      (list 'moore ((assigns 'fetch) 'daughter 'father 'moore))
                      (list 'parker ((assigns 'fetch) 'daughter 'father 'parker))
                      (list 'downing ((assigns 'fetch) 'daughter 'father 'downing))
                      (list 'hall ((assigns 'fetch) 'daughter 'father 'hall))))))



;--------------------------------------------------------------
;Section 4.3, Exercise 4.44

(interpret '(define (queens)
              (define (format input)
                (let ((letters '(a b c d e f g h)))
                  (define (iter letters numbers)
                    (cond ((null? letters) '())
                          (else (cons (list
                                       (car letters)
                                       (car numbers)) (iter (cdr letters) (cdr numbers))))))
                  (iter letters input)))
              
              (define (diagonal? new-file files)
                (define (iter files diff)
                  (cond ((null? files) 'false)
                        ((= (abs (- (car files) new-file)) diff) 'true)
                        (else (iter (cdr files) (+ diff 1)))))
                (iter files 1))
              (define (solve-iter result file)
                (if (> file 8) (format result)
                  (let ((next (amb 1 2 3 4 5 6 7 8)))
                      (begin
                        (require (not (memq next result)))
                        (require (not (diagonal? next result)))
                        (solve-iter (cons next result) (+ file 1))))))
              (solve-iter '() 1)))


;--------------------------------------------------------------
;Section 4.3, Exercise 4.48
;See parse-sentence code below




(define (interpret exp)
  (ambeval exp the-global-environment (lambda (val next-alternative) (display val) (newline)) (lambda () "failed")))

(interpret '(define (smallest-divisor n)
              (find-divisor n 2)))

(interpret '(define (find-divisor n test-divisor)
              (cond ((> (square test-divisor) n) 
                     n)
                    ((divides? test-divisor n) 
                     test-divisor)
                    (else (find-divisor 
                           n 
                           (+ test-divisor 1))))))

(interpret '(define (divides? a b)
              (= (remainder b a) 0)))

(interpret '(define (remainder a b)
              (cond ((< a b) a)
                    (else (remainder (- a b) b)))))

(interpret '(define (prime? n)
              (= n (smallest-divisor n))))

(interpret '(define (square x)
              (* x x)))


(interpret '(define (an-element-of items)
              (require (not (null? items)))
              (amb (car items) 
                   (an-element-of (cdr items)))))

(interpret '(define (prime-sum-pair list1 list2)
              (let ((a (an-element-of list1))
                    (b (an-element-of list2)))
                (require (prime? (+ a b)))
                (list a b))))

(interpret '(define (an-integer-starting-from n)
              (amb n (an-integer-starting-from (+ n 1)))))



(interpret '(define (a-pythagorean-triple-between low high)
              (let ((i (an-integer-between low high)))
                (let ((j (an-integer-between i high)))
                  (let ((k (an-integer-between j high)))
                    (require (= (+ (* i i) (* j j))
                                (* k k)))
                    (list i j k))))))

(interpret '(define (even? x)
              (cond ((= x 0) 'true)
                    ((< x 0) 'false)
                    (else (even? (- x 2))))))

(interpret '(define (if-fail-test items)
              (if-fail
               (let ((x (an-element-of items)))
                 (require (even? x))
                 x)
               (let ((x (an-element-of items)))
                 (require (not (even? x)))
                 x))))

(interpret '(define (an-element-of-except elements exclusions)
              (let ((element (an-element-of elements)))
                (define (excl-iter excl-list)
                  (if (not (null? excl-list))
                      (begin (require (not (= (car excl-list) element)))
                             (excl-iter (cdr excl-list)))))
                (excl-iter exclusions)
                element)))



(interpret '(define (memq item x)
              (cond ((null? x) false)
                    ((= item (car x)) x)
                    (else (member item (cdr x))))))
(interpret '(define (member item x)
              (cond ((null? x) false)
                    ((equal? item (car x)) x)
                    (else (member item (cdr x))))))

(interpret '(define (distinct? items)
              (cond ((null? items) true)
                    ((null? (cdr items)) true)
                    ((member (car items) (cdr items)) false)
                    (else (distinct? (cdr items))))))

(define (timed-interpret exp)
  (let ((a (runtime)))
    (interpret exp)
    (- (runtime) a)))

(define (average list)
  (/ (apply + list) (length list)))

(define (avg-timed-interpret exp times)
  (define (iter results n)
    (if (= n 0) results (iter (cons (timed-interpret exp) results) (- n 1))))
  (average (iter '() times)))


(interpret '
           (begin
             (define nouns
              '(noun student professor cat class))
             (define verbs
               '(verb studies lectures eats sleeps))
             (define articles '(article the a))
             (define prepositions
               '(prep for to in by with))
             (define adjectives
               '(adj red black hot cold orange))
             (define adverbs
               '(adv quickly annoyingly suddenly))
             (define conjunctions
               '(conj and or because))))

(interpret '(begin
              (define (map proc list)
                (if (null? list) '()
                    (cons (proc (car list)) (map proc (cdr list)))))
              
              (define (parse-clause)
                (list 'clause
                      (parse-noun-phrase)
                      (parse-verb-phrase)))

              (define (parse-conjunction)
                (parse-word conjunctions))

              (define (parse-sentence)
                (define (maybe-extend clause)
                  (amb
                   clause
                   (maybe-extend
                    (list 'compound
                          clause
                          (parse-conjunction)
                          (parse-clause)))))
                (maybe-extend (parse-clause)))
              
              (define (parse-verb-phrase)
                (define (maybe-extend verb-phrase)
                  (amb
                   verb-phrase
                   (maybe-extend
                    (list 'verb-phrase
                          verb-phrase
                          (parse-prepositional-phrase)))))
                (maybe-extend (parse-simple-verb-phrase)))

              (define (parse-adjective-phrase)
                (list 'adj-phrase
                      (parse-word adjectives)
                      (parse-word nouns)))
              
              (define (parse-simple-verb-phrase)
                (amb (parse-word verbs)
                     (list 'adv-phrase
                           (parse-word verbs)
                           (parse-word adverbs))))
             
              
              (define (parse-simple-noun-phrase)
                (list 'simple-noun-phrase
                      (parse-word articles)
                      (amb 
                       (parse-word nouns)
                       (parse-adjective-phrase))))
              
              (define (parse-noun-phrase)
                (define (maybe-extend noun-phrase)
                  (amb
                   noun-phrase
                   (maybe-extend
                    (list 'noun-phrase
                          noun-phrase
                          (parse-prepositional-phrase)))))
                (maybe-extend (parse-simple-noun-phrase)))
              
              (define (parse-prepositional-phrase)
                (list 'prep-phrase
                      (parse-word prepositions)
                      (parse-noun-phrase)))

              (define (list-amb list)
                (require (not (null? list)))
                (amb (car list) (list-amb (cdr list))))

              (define (quote x)
                (lambda () x))
              (define (length items)
                (if (null? items) 0
                    (+ 1 (length (cdr items)))))
              (define  (list-ref list id)
                (if (= id 0) (car list)
                    (list-ref (cdr list) (- id 1))))
              (define (list-less-than-n n)
                (define (iter counter result)
                  (if (= counter n) result
                      (iter (+ counter 1) (cons counter result))))
                (iter 0 '()))

              (define (list-ramb list)
                (let ((alternatives (list-less-than-n (length list))))
                  (let ((id (ambeval (cons 'ramb alternatives))))
                    (list-ref list id))))

              
              
              (define (parse-word-old word-list)
                (require (not (null? *unparsed*)))
                (require (member (car *unparsed*)
                                 (cdr word-list)))
                (let ((found-word (car *unparsed*)))
                  (set! *unparsed* (cdr *unparsed*))
                  (list (car word-list) found-word)))
              
              (define (parse-word word-list)
                (require (not (null? *unparsed*)))
                (require (member (car *unparsed*)
                                 (cdr word-list)))
                (let ((found-word (car *unparsed*)))
                  (set! *unparsed* (cdr *unparsed*))
                  (list (car word-list) (list-ramb (cdr word-list)))))

              (define *unparsed* '())
              
              (define (parse input)
                (set! *unparsed* input)
                (let ((sent (parse-sentence)))
                  (require (null? *unparsed*))
                  sent))))


(interpret '(define (prime-list)
              (let ((pairs '()))
                (if-fail
                 (let ((p (prime-sum-pair
                           '(1 3 5 8)
                           '(20 35 110))))
                   (permanent-set! pairs (cons p pairs))
                   (amb))
                 pairs))))




