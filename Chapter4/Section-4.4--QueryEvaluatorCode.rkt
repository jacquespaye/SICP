#lang sicp

(#%require "Section-4.4--QueryEvaluator.rkt")

;--------------------------------------------------------------
;Section 3.1, Exercise 4.57

(interpret
 '(assert! (rule (can-replace ?person-1 ?person-2)
                 (and (job ?person-1 ?job-1)
                      (job ?person-2 ?job-2)
                      (or (same ?job-1 ?job-2)
                          (can-do-job ?job-1 ?job-2))
                      (not (same ?person-1 ?person-2))))))

(interpret
 '(assert! (rule (same ?x ?x))))

;1
(interpret '(can-replace ?replacer (Fect Cy D)))

;2
;(interpret '(and (can-replace ?person-1 ?person-2)
 ;                (salary ?person-1 ?salary-1)
  ;               (salary ?person-2 ?salary-2)
   ;              (lisp-value < ?salary-1 ?salary-2)))


;--------------------------------------------------------------
;Section 3.1, Exercise 4.58

(interpret '(assert! (rule (big-shot ?person ?division)
                           (and (job ?person (?division . ?person-job))
                                (not (and (supervisor ?person ?boss)
                                          (job ?boss (?division . ?boss-job))))))))



;--------------------------------------------------------------
;Section 3.1, Exercise 4.59

(interpret-assertions
 (list
  '(meeting accounting (Monday 9am))
  '(meeting administration (Monday 10am))
  '(meeting computer (Wednesday 3pm))
  '(meeting administration (Friday 1pm))
  '(meeting whole-company (Wednesday 4pm))))

;1
;(meeting ?any (Friday ?time))

;2

;3
;should add the second portion so we specify the type of meeting
;(and (meeting-time (Hacker Alyssa P) (Wednesday . ?time)) (meeting ?div (Wednesday . ?time)))

(interpret '(assert!
             (rule (meeting-time ?person ?day-and-time)
                   (or (meeting whole-company ?day-and-time)
                        (and
                         (job ?person (?division . ?rest))
                         (meeting ?division ?day-and-time))))))



;--------------------------------------------------------------
;Section 3.1, Exercise 4.60
(interpret '(assert!
            (rule (lives-near ?person-1 ?person-2)
                  (and (address ?person-1 
                                (?town . ?rest-1))
                       (address ?person-2 
                                (?town . ?rest-2))
                       (not (same ?person-1 ?person-2))))))

(define (comp-names p1 p2)
  (let ((p1-string (symbol->string (car p1)))
        (p2-string (symbol->string (car p2))))
    (string>? p1-string p2-string)))

(interpret '(assert!
             (rule (lives-near-2 ?person-1 ?person-2)
                   (and (address ?person-1
                                 (?town . ?rest-1))
                        (address ?person-2
                                 (?town . ?rest-2))
                        (lisp-value comp-names ?person-1 ?person-2)))))


;--------------------------------------------------------------
;Section 3.1, Exercise 4.62
(interpret '(assert! (rule (last-pair (?x . ()) (?x . ())))))
(interpret '(assert! (rule (last-pair (?v . ?z) ?p)
                           (last-pair ?z ?p))))



;--------------------------------------------------------------
;Section 3.1, Exercise 4.63
(interpret '(assert! (rule (grandson ?g ?s)
                           (and (son ?g ?f)
                                (son ?f ?s)))))

(interpret '(assert! (rule (son ?f ?s)
                           (and (wife ?f ?w)
                                (son ?w ?s)))))


;--------------------------------------------------------------
;Section 3.1, Exercise 4.68

(interpret '(assert! (rule (reverse () ()))))


(interpret '(assert! (rule (reverse (?a . ?b) (?u . ?v))
                           (and (reverse ?b ?bb)
                                (append-to-form ?bb (?a) (?u . ?v)))
                           )))


;--------------------------------------------------------------
;Section 3.1, Exercise 4.69
(interpret '(assert! (rule (relate (grandson) ?p1 ?p2)
                           (grandson ?p1 ?p2))))

(interpret '(assert! (rule (relate (great . ?r) ?p1 ?p2)
                           (and (son ?p1 ?p1.5)
                                (relate ?r ?p1.5 ?p2)))))

(define (interpret x)
  (let ((q (query-syntax-process x)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! 
            (add-assertion-body q))
           (newline)
           (display 
            "Assertion added to data base."))
          (else
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))))))



(define (interpret-assertions items)
  (cond ((null? items) 'done)
        (else
         (interpret (list 'assert! (car items)))
              (interpret-assertions (cdr items)))))

(interpret-assertions
 (list 
  '(address (Bitdiddle Ben) 
            (Slumerville (Ridge Road) 10))
  '(job (Bitdiddle Ben) (computer wizard))
  '(salary (Bitdiddle Ben) 60000)
  '(address (Hacker Alyssa P) 
            (Cambridge (Mass Ave) 78))
  '(job (Hacker Alyssa P) (computer programmer))
  '(salary (Hacker Alyssa P) 40000)
  '(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

  '(address (Fect Cy D) 
            (Cambridge (Ames Street) 3))
  
  '(job (Fect Cy D) (computer programmer))
  '(salary (Fect Cy D) 35000)
  '(supervisor (Fect Cy D) (Bitdiddle Ben))

  '(address (Tweakit Lem E) 
            (Boston (Bay State Road) 22))
  '(job (Tweakit Lem E) (computer technician))
  '(salary (Tweakit Lem E) 25000)
  '(supervisor (Tweakit Lem E) (Bitdiddle Ben))
  '(address (Reasoner Louis) 
            (Slumerville (Pine Tree Road) 80))
  '(job (Reasoner Louis) 
        (computer programmer trainee))
  '(salary (Reasoner Louis) 30000)
  '(supervisor (Reasoner Louis) 
               (Hacker Alyssa P))
  '(supervisor (Bitdiddle Ben) (Warbucks Oliver))
  '(address (Warbucks Oliver) 
            (Swellesley (Top Heap Road)))
  '(job (Warbucks Oliver) 
        (administration big wheel))
  '(salary (Warbucks Oliver) 150000)
  '(address (Scrooge Eben) 
            (Weston (Shady Lane) 10))
  '(job (Scrooge Eben) 
        (accounting chief accountant))
  '(salary (Scrooge Eben) 75000)
  '(supervisor (Scrooge Eben) (Warbucks Oliver))

  '(address (Cratchet Robert) 
            (Allston (N Harvard Street) 16))
  '(job (Cratchet Robert) (accounting scrivener))
  '(salary (Cratchet Robert) 18000)
  '(supervisor (Cratchet Robert) (Scrooge Eben))
  '(address (Aull DeWitt) 
            (Slumerville (Onion Square) 5))
  '(job (Aull DeWitt) (administration secretary))
  '(salary (Aull DeWitt) 25000)
  '(supervisor (Aull DeWitt) (Warbucks Oliver))
  '(can-do-job (computer wizard) 
               (computer programmer))

  '(can-do-job (computer wizard) 
               (computer technician))
  '(can-do-job (computer programmer)
               (computer programmer trainee))
  '(can-do-job (administration secretary)
               (administration big wheel))
  '(son Adam Cain) '(son Cain Enoch)
  '(son Enoch Irad) '(son Irad Mehujael)
  '(son Mehujael Methushael)
  '(son Methushael Lamech)
  '(wife Lamech Ada) '(son Ada Jabal)
  '(son Ada Jubal)))



;(and (job person-1 ?job-1)
                    ;  (or (job person-2 ?job-1)
                     ;     (and (job person-2 ?job-2)
                      ;         (can-do-job ?job-1 ?job-2)
                       ;        (not (same ?person-1 ?person-2)))))






(interpret '(assert!
             (rule (append-to-form () ?y ?y))))
(interpret '(assert!
             (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                   (append-to-form ?v ?y ?z))))



(interpret '(assert! (rule (?x next-to ?y in (?x ?y . ?u)))))
(interpret '(assert! (rule (?x next-to ?y in (?v . ?z))
                           (?x next-to ?y in ?z))))


(interpret '(assert! (rule (wheel ?person)
                           (and (supervisor ?middle-manager ?person)
                                (supervisor ?x ?middle-manager)))))


(interpret '(assert! (married Minnie Mickey)))

(interpret '(assert! (rule (married ?x ?y)
               (married ?y ?x))))

(interpret '(assert! (rule (outranked-by ?staff-person ?boss)
                           (or (supervisor ?staff-person ?boss)
                               (and (outranked-by ?middle-manager
                                                  ?boss)
                                    (supervisor ?staff-person 
                                                ?middle-manager))))))

(interpret '(assert! (rule (or-test ?x ?y)
                           (or (address ?x ?y)
                               (or-test ?x ?y)))))


(interpret '(assert! (rule (interleave-test ?x ?y)
                           (or (or-test ?x ?y)
                               (supervisor ?x ?y)))))

(interpret '(assert! (rule (sq-interleave ?x ?y)
                           (and (job ?x (computer . ?z))
                                (or-test ?x ?y)))))

(interpret '(assert! (rule (flatten-test ?u ?v)
                           (and (or-test ?u ?z)
                                (job ?u ?v)))))

(interpret '(assert! (rule (flatten-test-2 ?u ?z)
                           (and (or-test ?u ?z)
                                (not (job ?u (computer . ?v)))))))






