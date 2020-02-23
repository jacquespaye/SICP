#lang sicp
(#%provide (all-defined))

;--------------------------------------------------------------
;Section 3.1, Exercise 4.67

;See simple-loop-check code below



;--------------------------------------------------------------
;Section 3.1, Exercise 4.74

;See simple-stream-flatmap code below


;--------------------------------------------------------------
;Section 3.1, Exercise 4.75

;See uniquely-asserted definition below

;--------------------------------------------------------------
;Section 3.1, Exercise 4.76

;See conjoin code below, which is placed in data directed table under 'and

;--------------------------------------------------------------
;Section 3.1, Exercise 4.77


;See force-stream-deferrals, force-deferrals, refresh-stream-deferrals, and refresh-deferrals
;in code below


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))


(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))


(define (display-line x)
  (newline)
  (display x))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))




;code copied from stackoverflow

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))


(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (stream-append (stream-cdr s1) s2))))


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

;end of  stackoverflow



;code for put and get
;;implementation of the put/get procedures (treated as an abstraction here)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))



;end of code for put and get

(define input-prompt  ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (clear-history)
  (set! THE-HISTORY '()))

(define (simple-loop-check query frame-stream)
  (if (check-loop query frame-stream THE-HISTORY)
      (error "Loop checker detected a loop!" query)
      (add-to-history query frame-stream)))

(define (refresh-stream-deferrals frame-stream)
  (stream-flatmap
   (lambda (frame)
     (refresh-deferrals frame))
   frame-stream))

(define (refresh-deferrals frame)
  (display "\n\nREFRESHING")
  (display "\nFRAME:") (display frame)
  (define (refresh-iter frame result)
    (cond ((null? frame) (singleton-stream (reverse result)))
          ((deferral? (car frame))
           (refresh-iter
            (cdr frame)
            (cons
             (if ((cdar frame) 'HAS-RUN?)
                 (car frame)
                 (cons
                  (caar frame);(get-new-deferral-var)
                  ((cdar frame) 'EXTENDING!))) result)))
          (else (refresh-iter (cdr frame) (cons (car frame) result)))))
  (refresh-iter frame '()))


(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! 
            (add-assertion-body q))
           (newline)
           (display 
            "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (clear-history)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate
                   q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (force-stream-deferrals  
              (qeval q (singleton-stream '())))))
           (query-driver-loop)))))


(define (instantiate 
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
  (copy exp))

(define (binding-exists? e f)
  (not (eq? #f (binding-in-frame e f))))

(define (bound? e1 e2 f)
  (let ((value (binding-value (binding-in-frame e1 f))))
    (cond
      ((equal? value e2) #t)
      ((binding-exists? value f) (bound? value e2 f))
      (else #f))))

(define excluded-forms '(always-true))

(define (equivalent? e1 e2 f1 f2)
  (cond ((binding-exists? e1 f1) (bound? e1 e2 f1))
        ((binding-exists? e2 f1) (bound? e2 e1 f1))
        ((member e1 excluded-forms) #f)
        (else (and (not (binding-exists? e1 f1)) (not (binding-exists? e2 f2))))))

(define (comp-qfs q1 q2 f1 f2)
  (define (qfs-iter q1 q2)
    (if (and (null? q1) (null? q2)) #t
        (let ((elem1 (car q1))
              (elem2 (car q2)))
          (cond ((and (not (var? elem1)) (not (equal? elem1 elem2))) #f)
                ((not (equivalent? elem1 elem2 f1 f2)) #f)
                (else (qfs-iter (cdr q1) (cdr q2)))))))
  (qfs-iter q1 q2))

(define (make-hist-entry query frame)
  (list query frame))
(define (hist-query hist-entry)
  (car hist-entry))
(define (hist-frame hist-entry)
  (cadr hist-entry))

(define (check-history frame query history)
  (define (hist-iter hist)
    (if (null? hist) #f
        (let* ((first-entry (car hist))
               (first-q (hist-query first-entry))
               (first-f (hist-frame first-entry)))
          (if (comp-qfs query first-q frame first-f) #t
              (begin (hist-iter (stream-cdr hist)))))))
  (hist-iter history))

(define (check-loop query frame-stream history)
  (define (check-iter frames)
    (cond ((stream-null? frames) #f)
          ((check-history (stream-car frames) query history) #t)
          (else (check-iter (stream-cdr frames)))))
  (check-iter frame-stream))

(define (make-history)
  '())


(define THE-HISTORY (make-history))

(define (add-to-history query frame-stream)
  (let ((old-history THE-HISTORY))
    (set! THE-HISTORY
          (stream-append
           (stream-map (lambda (frame) (make-hist-entry query frame)) frame-stream)
           old-history))))


(define (qeval query frame-stream)
  (let*
      ((frame-stream-forced (force-stream-deferrals frame-stream))
       )
    (simple-loop-check query frame-stream-forced)
    (let ((qproc (get (type query) 'qeval)))
      (if qproc
          (qproc (contents query) frame-stream-forced)
          (simple-query query frame-stream-forced)))))

(define (simple-query query-pattern 
                      frame-stream)
  (stream-flatmap
   (lambda (frame)
       (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay 
          (apply-rules query-pattern frame))))
   frame-stream))


(define (conjoin-old conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin-old (rest-conjuncts conjuncts)
               (qeval 
                (first-conjunct conjuncts)
                frame-stream))))

(define (first-binding frame)
  (car frame))
(define (rest-bindings frame)
  (cdr frame))

(define (compatible-pairs frame-stream-1 frame-stream-2)
  (define (attempt-to-reconcile f1 f2)
    (define (reconc-iter f1 f2)
      (if (null? f1) (singleton-stream f2)
          (let* ((f1-b1 (first-binding f1))
                 (b1-var (binding-variable f1-b1))
                 (b1-val (binding-value f1-b1))
                 (f2-extended (unify-match b1-var b1-val f2)))
            (if (eq? f2-extended 'failed) the-empty-stream
                (reconc-iter (rest-bindings f1) f2-extended)))))
    (reconc-iter f1 f2))
  
  (stream-flatmap
              (lambda (fs1-item)
                (stream-flatmap
                            (lambda (fs2-item)
                              (attempt-to-reconcile fs1-item fs2-item))
                            frame-stream-2))
              frame-stream-1))

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((frame-stream-2
             (qeval (first-conjunct conjuncts) frame-stream)))
        (conjoin (rest-conjuncts conjuncts)
                 (compatible-pairs frame-stream frame-stream-2)))))
       

(put 'and 'qeval conjoin)

(define (sum-variable sum-structure) (car sum-structure))
(define (sum-unique sum-structure) (cadr sum-structure))
(define (sum-query sum-structure) (caddr sum-structure))

(define (sum-accum results-stream)
  (display results-stream))


(define (stream-sum-if-unique variable unique stream)
  (define (iter stream result uniques)
    (if (stream-null? stream) result
        (let* ((first-frame (car stream))
               (variable-value (binding-value (binding-in-frame variable first-frame)))
               (unique-value (binding-value (binding-in-frame unique first-frame))))
          (display unique-value)
          (if (memq unique-value uniques)
              (iter (stream-cdr stream) result uniques)
              (iter (stream-cdr stream) (+ variable-value result) (cons unique-value uniques))))))
  (iter stream 0 '()))

(define (member item list)
  (cond ((null? list) #f)
        ((equal? (car list) item) list)
        (else (member item (cdr list)))))

(define (stream-sum-if-unique-2 instantiated-stream)
  (define (iter stream result taken)
    (if (stream-null? stream) result
        (let* ((first-frame (car stream))
               (first-value (car first-frame))
               (first-unique (cadr first-frame)))
          (if (member first-unique taken)
              (iter (stream-cdr stream) result taken)
              (iter (stream-cdr stream) (+ first-value result) (cons first-unique taken))))))
  (iter instantiated-stream 0 '()))
  
  

(define (sum-process sum-structure frame-stream)
  (let* ((variable (sum-variable sum-structure))
        (unique (sum-unique sum-structure))
        (q (sum-query sum-structure))
        (results-stream (qeval q frame-stream))
        (instantiated-stream             (stream-map
                                          (lambda (frame)
                                            (instantiate
                                                sum-structure
                                              frame
                                              (lambda (v f)
                                                (contract-question-mark v))))
                                          results-stream)))
    
    (display "\nYour result is:  ") (display (stream-sum-if-unique-2 instantiated-stream)) (display "\n")
    results-stream))

(put 'sum 'qeval sum-process)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) 
              frame-stream)
       (delay (disjoin 
               (rest-disjuncts disjuncts)
               frame-stream)))))
(put 'or 'qeval disjoin)

(define def-counter 0)

(define (new-deferral-id)
  (set! def-counter (+ def-counter 1))
  def-counter)

(define (get-new-deferral-var)
  (list '?
        (new-deferral-id)
        '*DEFERRAL*))

(define (make-deferred-entry deferred-proc)
  (make-binding
   (get-new-deferral-var)
   deferred-proc))

(define (deferral? binding)
  (and
   (pair? binding)
   (pair? (car binding))
   (= (length (car binding)) 3)
   (eq? (caddar binding) '*DEFERRAL*)))

(define (deferral-proc binding)
  (cdr binding))

(define (force-stream-deferrals frame-stream)
  (let* ((refreshed-stream frame-stream);(refresh-stream-deferrals frame-stream))
         (returned-flatmap (simple-stream-flatmap
                            (lambda (frame)
                              (force-deferrals frame))
                            refreshed-stream)))
    returned-flatmap))



(define (force-deferrals frame)
  (define (iter sub-frame frame-output)
    (display "FORCING DEFERRALS")
    (display sub-frame)
    (if (null? sub-frame) (singleton-stream frame-output)
        (let ((first-binding (car sub-frame)))
          (cond ((deferral? first-binding) 
                                           (let ((new-stream ((deferral-proc first-binding) frame-output)))
                                                    (if (stream-null? new-stream) the-empty-stream
                                                        (iter (cdr sub-frame) (stream-car new-stream)))))
                (else (iter (cdr sub-frame) frame-output))))))
  (iter frame frame))

        
        
(define (add-deferred-operation deferred-proc frame)
  (let ((def-entry (make-deferred-entry
                    deferred-proc)))
    (singleton-stream (cons def-entry frame))))


(define (make-deferred-proc operands frame-query-function has-run?)
  (lambda (frame)
    (cond ((eq? frame 'EXTENDING!) (begin (let ((new-has-run? has-run?))
                                            (make-deferred-proc operands frame-query-function new-has-run?))))
          ((eq? frame 'HAS-RUN?) has-run?)
          ;(has-run? (singleton-stream frame))
          ((enough-bindings? (car operands) frame)
           (set! has-run? #t)
           (frame-query-function frame operands))
          (else (singleton-stream frame)))))

(define (enough-bindings? query frame)
  (if (null? query) (begin #t)
      (let ((first (car query)))
        (cond ((var? first)
               (if (binding-in-frame first frame)
                   (enough-bindings? (cdr query) frame)
                   #f))
              (else (enough-bindings? (cdr query) frame))))))
  

(define frame-query-function-negate
  (lambda (frame operands)
    (if (stream-null?
         (qeval (negated-query operands)
                (singleton-stream frame)))
        (singleton-stream frame)
        the-empty-stream)))


(define (negate-old operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (stream-null? 
          (qeval (negated-query operands)
                 (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (negate operands frame-stream)
  ;(force-stream-deferrals
   (simple-stream-flatmap
    (lambda (frame)
      (add-deferred-operation (make-deferred-proc operands frame-query-function-negate #f)
                              frame))
    frame-stream));)

(put 'not 'qeval negate)


(define (unique-query contents)
  (car contents))

(define (uniquely-asserted contents frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (let ((evaluated (qeval (unique-query contents) (singleton-stream frame))))
       (cond ((stream-null? evaluated) the-empty-stream)
             ((stream-null? (stream-cdr evaluated)) evaluated)
             (else the-empty-stream))))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)

(define frame-query-function-lisp-value
  (lambda (frame operands)
    (if (execute
          (instantiate
           (car operands)
           frame
           (lambda (v f)
             (error 
              "Unknown pat var: LISP-VALUE" 
              v))))
         (singleton-stream frame)
         the-empty-stream)))



(define (lisp-value operands frame-stream)
  ;(force-stream-deferrals
   (simple-stream-flatmap
    (lambda (frame)
      (add-deferred-operation (make-deferred-proc (list operands) frame-query-function-lisp-value #f)
                              frame))
    frame-stream));)

(define (lisp-value-old call frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error 
              "Unknown pat var: LISP-VALUE" 
              v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) 
              (interaction-environment))
         (args exp)))

(define (always-true ignore frame-stream) 
  frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (simple-stream-flatmap 
    (lambda (datum) 
      (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

(define (check-an-assertion 
         assertion query-pat query-frame)
  (let ((match-result
         (pattern-match 
          query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) 
         (extend-if-consistent 
          pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match 
          (cdr pat) 
          (cdr dat)
          (pattern-match
           (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match 
         (binding-value binding) dat frame)
        (extend var dat frame))))


(define (apply-rules pattern frame)
  (stream-flatmap 
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))


(define (apply-a-rule rule
                      query-pattern
                      query-frame)
  (let ((clean-rule 
         (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream 
                  unify-result))))))



(define (rename-variables-in rule)
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
    (tree-walk rule)))


(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
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
        (else 'failed)))


(define (extend-if-possible var val frame)
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
           'failed)
          (else (extend var val frame)))))


(define (depends-on? exp var frame)
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
  (tree-walk exp))



(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern)
              'assertion-stream))



(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern)
               'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion 
                       old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES
          (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream 
                key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream 
                assertion
                current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream 
                  key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream 
                  rule
                  current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1)
                              delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed 
        (force delayed-s2)
        (delay (stream-cdr s1))))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream
               (stream-cdr stream))))))

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

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (s) (not (stream-null? s)))
                             stream)))



(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE"
             exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS"
             exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))


(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols 
                proc (car exp))
               (map-over-symbols 
                proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '? (string->symbol
                  (substring
                   chars 
                   1 
                   (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable 
         var rule-application-id)
  (cons '? (cons rule-application-id
                 (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr variable))
         (string-append
          (symbol->string (caddr variable))
          "-"
          (number->string (cadr variable)))
         (symbol->string (cadr variable))))))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))








