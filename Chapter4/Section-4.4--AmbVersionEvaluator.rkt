#lang sicp
(#%provide (all-defined))


;--------------------------------------------------------------
;Section 3.1, Exercise 4.78
;See below for implementation of the ambiguous evaluator used
;to run the query language


(define apply-in-underlying-scheme apply)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))


(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr variable))
         (string-append
          (symbol->string (caddr variable))
          "-"
          (number->string (cadr variable)))
         (symbol->string (cadr variable))))))


(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't 
                        last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))

(define (true? x)
  (not (eq? x 'false)))

(define (false? x)
  (eq? x 'false))
(define (make-procedure parameters body env)
    (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (make-set set-variable set-body)
  (list 'set! set-variable set-body))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (boolConvert oldBool)
  (cond ((eq? oldBool #f) 'false)
        ((eq? oldBool #t) 'true)
        (else oldBool)))

(define (boolConvert-back oldBool)
  (cond ((eq? oldBool 'false) #f)
        ((eq? oldBool 'true) #t)
        (else oldBool)))

(define (convertBoolProc proc)
  (define (newBoolProc . terms)
    (let ((oldBool (apply proc terms)))
      (boolConvert oldBool)))
  newBoolProc)

(define (convertBoolProc-back proc)
  (define (newBoolProc . terms)
    (let ((oldBool (apply proc terms)))
      (boolConvert-back oldBool)))
  newBoolProc)

(define new= (convertBoolProc =))
(define new-eq? (convertBoolProc eq?))
(define new-equal? (convertBoolProc equal?))
(define new-null? (convertBoolProc null?))
(define (new-not item)
  (if (false? item) 'true 'false))
(define new<
  (convertBoolProc <))
(define new>
  (convertBoolProc >))
(define new<=
  (convertBoolProc <=))



(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))


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

(define and2
  (convertBoolProc (lambda (a b) (and (boolConvert-back a) (boolConvert-back b)))))


(define or2
  (convertBoolProc (lambda (a b) (or (boolConvert-back a) (boolConvert-back b)))))

(define pair2
  (convertBoolProc pair?))

(define assoc2
  (convertBoolProc assoc))


(define primitive-procedures
  (list
   (list 'query-syntax-process query-syntax-process)
   (list 'car car)
   (list 'contract-question-mark contract-question-mark)
   (list 'assoc assoc2)
   (list 'pair? pair2)
   (list 'and and2)
   (list 'or or2)
   (list 'cddr cddr)
   (list 'caddr caddr)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? new-null?)
   (list 'not new-not)
   (list '* *)
   (list '/ /)
   (list '+ +)
   (list '- -)
   (list '= new=)
   (list 'equal? new-equal?)
   (list 'eq? new-eq?)
   (list 'cadr cadr)
   (list 'caar caar)
   (list '< new<)
   (list '> new>)
   (list 'list list)
   (list 'append append)
   (list 'display display)
   (list '<= new<=)
   (list 'abs abs)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))
(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true 'true initial-env)
    (define-variable! 'false 'false initial-env)
    (define-variable! ''() '() initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;;AMB PROCEDURES;;;
(define (amb? exp) (tagged-list? exp 'amb))
(define (ramb? exp) (tagged-list? exp 'ramb))

(define (amb-choices exp) (cdr exp))
(define (ramb-choices exp) (cdr exp))


(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignment exp))
        ((definition? exp)
         (analyze-definition exp))
        ((if? exp)
         (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence
          (begin-actions exp)))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((let? exp)
         (analyze (let->combination exp)))
        ((amb? exp)
         (analyze-amb exp))
        ((ramb? exp)
         (analyze-ramb exp))
        ((apply? exp)
         (analyze-apply exp))
        ((ambeval? exp)
         (analyze-ambeval exp))
        ((permanent-set? exp)
         (analyze-perm-assign exp))
        ((if-fail? exp)
         (analyze-if-fail exp))
        ((require? exp)
         (analyze-require exp))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Unknown expression type: ANALYZE" exp))))

(define (require? exp)
  (tagged-list? exp 'require))

(define (require-predicate exp)
  (cadr exp))

(define (analyze-require exp)
  (let ((pred-proc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pred-proc
       env
       (lambda (pred-value fail2)
         (if (false? pred-value)
             (fail)
             (succeed 'ok fail2)))
       fail))))
      

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (permanent-set? exp)
  (tagged-list? exp 'permanent-set!))

(define (ambeval? exp)
  (tagged-list? exp 'ambeval))
(define (apply? exp)
  (tagged-list? exp 'apply))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-clauses exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let-vars exp)
  (map car (let-clauses exp)))

(define (let-values exp)
  (map cadr (let-clauses exp)))

(define (let->combination exp)
  (cons
   (make-lambda
    (let-vars exp)
    (let-body exp))
   (let-values exp)))
  

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (perm-assignment-variable exp)
  (cadr exp))

(define (perm-assignment-value exp)
  (caddr exp))

(define (if-fail-primary exp)
  (cadr exp))
(define (if-fail-secondary exp)
  (caddr exp))


(define (analyze-if-fail exp)
  (let ((primary (analyze (if-fail-primary exp)))
        (secondary (analyze (if-fail-secondary exp))))
    (lambda (env succeed fail)
      (primary
       env
       (lambda (primary-value fail2)
         (succeed primary-value fail2))
       (lambda ()
         (secondary
          env
          (lambda (secondary-value fail3)
            (succeed secondary-value fail3))
          fail))))))



(define (analyze-perm-assign exp)
  (let ((var (perm-assignment-variable exp))
        (vproc (analyze (perm-assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed
                'ok
                fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value
                       var
                       env)))
                 (set-variable-value!
                  var
                  val
                  env)
                 (succeed
                  'ok
                  (lambda ()
                    (set-variable-value!
                     var
                     old-value
                     env)
                    (fail2)))))
             fail))))
    



(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze
                (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence
                (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
            (a env
               (lambda (a-value fail2)
                 (b env succeed fail2))
               fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args
                aprocs
                env
                (lambda (args fail3)
                  (execute-application
                   proc args succeed fail3))
                fail2))
             fail))))
(define (apply-operator exp)
  (cadr exp))
(define (apply-operands-list exp)
  (caddr exp))

(define (analyze-ambeval exp)
  (lambda (env succeed fail)
    (ambeval (cadr exp)
             env
             (lambda (exp-list fail2)
               (ambeval exp-list env succeed fail2))
             fail)))

(define (analyze-apply exp)
  (let ((fproc (analyze (apply-operator exp)))
        (argsproc (analyze (apply-operands-list exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (argsproc
                env
                (lambda (args fail3)
                  (execute-application
                   proc args succeed fail3))
                fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;;success continuation
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          ;;success continuation for
          ;;recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args)
                     fail3))
          fail2))
       fail)))


(define (execute-application
         proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed
          (apply-primitive-procedure
           proc args)
          fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
         (extend-environment
          (procedure-parameters proc)
          args
          (procedure-environment proc))
         succeed
         fail))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

(define (analyze-amb exp)
  (let ((cprocs
         (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))


(define (random-except n exceptions)
  (define (all-except items exceptions)
    (cond ((null? items) '())
          ((memq (car items) exceptions) (all-except (cdr items) exceptions))
          (else (cons (car items) (all-except (cdr items) exceptions)))))
  (define (list-less-than-n n)
    (define (iter counter result)
      (if (= counter n) result
          (iter (+ counter 1) (cons counter result))))
    (iter 0 '()))
  (let ((ok-items (all-except (list-less-than-n n) exceptions)))
    (if (not (null? ok-items))
        (list-ref ok-items (random (length ok-items)))
        (error "No permissible alternatives from which to select a random item!"))))



(define (analyze-ramb exp)
  (let ((cprocs
         (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices length-choices except-ids)
        (if (= length-choices (length except-ids))
            (fail)
            (let ((random-id (random-except length-choices except-ids)))
            ((list-ref choices random-id)
             env
             succeed
             (lambda ()
               (try-next choices length-choices (cons random-id except-ids)))))))
      (try-next cprocs (length cprocs) '()))))



(define (apply-proc exp)
  (cadr exp))
(define (apply-args exp)
  (caddr exp))

(define (analyze-apply-old exp)
  (lambda (env succeed fail)
    (ambeval (apply-args exp)
             env
             (lambda (args fail2)
               (ambeval
                (cons (apply-proc exp) args)
                env
                succeed
                fail2))
             fail)))




(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display
             ";;; Starting a new problem ")
            (ambeval
             input
             the-global-environment
             (lambda (val next-alternative)
               (announce-output
                output-prompt)
               (user-print val)
               (internal-loop
                next-alternative))
             (lambda ()
               (announce-output
                ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))





  
