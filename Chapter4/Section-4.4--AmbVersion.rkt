#lang sicp

(#%require "Section-4.4--AmbVersionEvaluator.rkt")
(#%require schemeunit)



;--------------------------------------------------------------
;Section 3.1, Exercise 4.78
;See below for implementation of the query language as a program for the
;ambiguous evaluator

;The main observed difference is that this evaluator goes through the possibilities in a tree-like order, going from left to right
;when evaluating "and" for example.





(define (interpret exp)
  (ambeval exp the-global-environment (lambda (val next-alternative) (display val) (newline)) (lambda () "failed")))

(interpret '(define THE-ASSERTIONS '()))
(interpret '(define (add-assertion item)
              (set! THE-ASSERTIONS (cons item THE-ASSERTIONS))))

(interpret '(define THE-RULES '()))
(interpret '(define (add-rule item)
              (set! THE-RULES (cons item THE-RULES))))

(interpret '(define (process-rule rule)
              (query-syntax-process (cdr rule))))

(define (interpret-assertions items)
    (if (null? items) 'DONE
      (begin (interpret (list 'add-assertion (car items)))
             (interpret-assertions (cdr items)))))

(define (interpret-rules items)
    (if (null? items) 'DONE
      (begin (interpret (list 'add-rule (car items)))
             (interpret-rules (cdr items)))))

(interpret-rules
 (list ''(rule (grandson ?g ?s)
                           (and (son ?g ?f)
                                (son ?f ?s)))
       ''(rule (son ?f ?s)
                           (and (wife ?f ?w)
                                (son ?w ?s)))
       ''(rule (append-to-form () ?y ?y))
       ''(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))
       ''(rule (wheel ?person)
                           (and (supervisor ?middle-manager ?person)
                                (supervisor ?x ?middle-manager)))
       ''(rule (married ?x ?y)
               (married ?y ?x))
       ''(rule (meeting-time ?person ?day-and-time)
                   (or (meeting whole-company ?day-and-time)
                        (and
                         (job ?person (?division . ?rest))
                         (meeting ?division ?day-and-time))))
       ''(rule (reverse (?a . ?b) (?u . ?v))
                           (and (reverse ?b ?bb)
                                (append-to-form ?bb (?a) (?u . ?v)))
                           )
       ''(rule (reverse () ()))))

(interpret-assertions
  (list 
  ''(address (Bitdiddle Ben) 
            (Slumerville (Ridge Road) 10))
  ''(job (Bitdiddle Ben) (computer wizard))
  ''(salary (Bitdiddle Ben) 60000)
  ''(married Minnie Mickey)
  ''(address (Hacker Alyssa P) 
            (Cambridge (Mass Ave) 78))
  ''(job (Hacker Alyssa P) (computer programmer))
  ''(salary (Hacker Alyssa P) 40000)
  ''(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

  ''(address (Fect Cy D) 
            (Cambridge (Ames Street) 3))
  
  ''(job (Fect Cy D) (computer programmer))
  ''(salary (Fect Cy D) 35000)
  ''(supervisor (Fect Cy D) (Bitdiddle Ben))

  ''(address (Tweakit Lem E) 
            (Boston (Bay State Road) 22))
  ''(job (Tweakit Lem E) (computer technician))
  ''(salary (Tweakit Lem E) 25000)
  ''(supervisor (Tweakit Lem E) (Bitdiddle Ben))
  ''(address (Reasoner Louis) 
            (Slumerville (Pine Tree Road) 80))
  ''(job (Reasoner Louis) 
        (computer programmer trainee))
  ''(salary (Reasoner Louis) 30000)
  ''(supervisor (Reasoner Louis) 
               (Hacker Alyssa P))
  ''(supervisor (Bitdiddle Ben) (Warbucks Oliver))
  ''(address (Warbucks Oliver) 
            (Swellesley (Top Heap Road)))
  ''(job (Warbucks Oliver) 
        (administration big wheel))
  ''(salary (Warbucks Oliver) 150000)
  ''(address (Scrooge Eben) 
            (Weston (Shady Lane) 10))
  ''(job (Scrooge Eben) 
        (accounting chief accountant))
  ''(salary (Scrooge Eben) 75000)
  ''(supervisor (Scrooge Eben) (Warbucks Oliver))

  ''(address (Cratchet Robert) 
            (Allston (N Harvard Street) 16))
  ''(job (Cratchet Robert) (accounting scrivener))
  ''(salary (Cratchet Robert) 18000)
  ''(supervisor (Cratchet Robert) (Scrooge Eben))
  ''(address (Aull DeWitt) 
            (Slumerville (Onion Square) 5))
  ''(job (Aull DeWitt) (administration secretary))
  ''(salary (Aull DeWitt) 25000)
  ''(supervisor (Aull DeWitt) (Warbucks Oliver))
  ''(can-do-job (computer wizard) 
               (computer programmer))

  ''(can-do-job (computer wizard) 
               (computer technician))
  ''(can-do-job (computer programmer)
               (computer programmer trainee))
  ''(can-do-job (administration secretary)
               (administration big wheel))
  ''(son Adam Cain) ''(son Cain Enoch)
  ''(son Enoch Irad) ''(son Irad Mehujael)
  ''(son Mehujael Methushael)
  ''(son Methushael Lamech)
  ''(wife Lamech Ada) ''(son Ada Jabal)
  ''(son Ada Jubal)
   ''(meeting accounting (Monday 9am))
  ''(meeting administration (Monday 10am))
  ''(meeting computer (Wednesday 3pm))
  ''(meeting administration (Friday 1pm))
  ''(meeting whole-company (Wednesday 4pm))))

(interpret '(define (var? exp) (tagged-list? exp '?)))
(interpret '(define (binding-in-frame variable frame)
              (assoc variable frame)))
(interpret '(define (binding-variable binding)
              (car binding)))

(interpret '(define (binding-value binding)
              (cdr binding)))


(interpret '(define (extend variable value frame)
              (cons (make-binding variable value) frame)))

(interpret '(define (make-binding variable value)
              (cons variable value)))

  
(interpret '(define (pattern-match pat dat frame)
              (cond ((equal? pat dat) frame)
                    ((var? pat) 
                     (extend-if-consistent 
                      pat dat frame))
                    ((and (pair? pat) (pair? dat))
                     (pattern-match 
                      (cdr pat) 
                      (cdr dat)
                      (pattern-match
                       (car pat) (car dat) frame)))
                    (else (amb)))))

(interpret '(define (extend-if-consistent var dat frame)
              (let ((binding (binding-in-frame var frame)))
                (if binding
                    (pattern-match 
                     (binding-value binding) dat frame)
                    (extend var dat frame)))))

(interpret '(define (tagged-list? exp tag)
              (if (pair? exp)
                  (eq? (car exp) tag)
                  false)))

(interpret '(define (compare q assertions frame)
              (if (null? assertions) (amb)
                  (amb (pattern-match q (car assertions) frame);;change this to eventually contract qmark
                       (compare q (cdr assertions) frame)))))

(interpret '(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                   ; ***
           (let ((binding 
                  (binding-in-frame 
                   val
                   frame)))
             (if binding
                 (unify-match
                  var 
                  (binding-value binding) 
                  frame)
                 (extend var val frame))))
          ((depends-on? val var frame)  ; ***
           (amb))
          (else (extend var val frame))))))

(interpret '(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let
                 ((b (binding-in-frame 
                      e 
                      frame)))
                  (if b
                      (tree-walk 
                       (binding-value b))
                      false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp)))


(interpret '(define (apply-a-rule rule
                                  query-pattern
                                  query-frame)
              (let ((clean-rule 
                     (rename-variables-in rule)))
                (let ((unify-result
                       (unify-match query-pattern
                                    (conclusion clean-rule)
                                    query-frame)))
                  (display "\n UNIFY RESULT:") (display unify-result)
                  (display "\n BODY:") (display (rule-body clean-rule))
                  (qeval (rule-body clean-rule)
                         unify-result)))))
(interpret '(define (conclusion rule) (car rule)))

(interpret '(define (rule-body rule)
  (if (null? (cdr rule))
      '(always-true)
      (cadr rule))))

(interpret '(define (rename-variables-in rule)
  (let ((rule-application-id 
         (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable 
              exp 
              rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule))))


(interpret '(define rule-counter 0))

(interpret '(define (new-rule-application-id)
              (set! rule-counter (+ 1 rule-counter))
              rule-counter))

(interpret '(define (unify-match p1 p2 frame)
              (cond ((equal? p1 p2) frame)
                    ((var? p1)
                     (extend-if-possible p1 p2 frame))
                    ((var? p2)
                     (extend-if-possible 
                      p2 
                      p1 
                      frame))        ; ***
                    ((and (pair? p1) 
                          (pair? p2))
                     (unify-match 
                      (cdr p1) 
                      (cdr p2)
                      (unify-match 
                       (car p1)
                       (car p2)
                       frame)))
                    (else (amb)))))

(interpret '(define (make-new-variable 
         var rule-application-id)
  (cons '? (cons rule-application-id
                 (cdr var)))))

(interpret '(define (unify query rules frame)
              (if (null? rules) (amb)
                  (amb (apply-a-rule (process-rule (car rules)) query frame) (unify query (cdr rules) frame)))))

(interpret '(define (rule-eval query frame)
              (unify query THE-RULES frame)))

(interpret '(define (conjoin items frame)
              (display "\n\nCONJOINING ITEMS:") (display items)
              (display "\nCONJOINING FRAME:") (display frame)
              (if (null? items) frame
                  (conjoin (cdr items) (qeval (car items) frame)))))

(interpret '(define (disjoin items frame)
              (if (null? items) (ramb)
                  (ramb (qeval (car items) frame) (disjoin (cdr items) frame)))))

(interpret '(define (assertion-eval query frame)
                (compare query THE-ASSERTIONS frame)))

(interpret '(define (qeval query frame)
              (cond ((tagged-list? query 'and) (conjoin (cdr query) frame))
                    ((tagged-list? query 'or) (disjoin (cdr query) frame))
                    ((equal? query '(always-true)) frame)
                    (else (amb (assertion-eval query frame) (rule-eval query frame))))))

(interpret '(define (qdrive query)
              (let ((q (query-syntax-process query)))
                (let ((output-frame
                       (qeval q '())))
                  (instantiate q output-frame (lambda (v f)
                                                (contract-question-mark v)))))))




(interpret '(define (instantiate 
                        exp frame unbound-var-handler)
              (define (copy exp)
                (cond ((var? exp)
                       (let ((binding 
                              (binding-in-frame 
                               exp frame)))
                         (if binding
                             (copy 
                              (binding-value binding))
                             (unbound-var-handler 
                              exp frame))))
                      ((pair? exp)
                       (cons (copy (car exp)) 
                             (copy (cdr exp))))
                      (else exp)))
              (copy exp)))



