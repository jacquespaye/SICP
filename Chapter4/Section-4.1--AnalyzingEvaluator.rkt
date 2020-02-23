#lang sicp
(#%provide (all-defined))

;--------------------------------------------------------------
;Section 4.1, Exercise 4.22
;See eval-let usage in the analyzing evaluator below


;--------------------------------------------------------------
;Section 4.1, Exercise 4.24

;The procedures below can be used to do the test both the analyzing and the metacircular
;evaluator
(define (interpret x)
  (eval x the-global-environment))


(define (timed-eval exp)
  (let ((a (runtime)))
    (eval exp the-global-environment)
    (- (runtime) a)))


(interpret '(define (fib n) 
              (fib-iter 1 0 n)))
(interpret '(define (fib-iter a b count)
              (if (= count 0)
                  b
                  (fib-iter (+ a b) a (- count 1)))))




;--------------------------------------------------------------
;Below find the implementation of the analyzing evaluator,
;it can be started with (driver-loop)


(define apply-in-underlying-scheme apply)


(define (type exp) (car exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp) 
         (lookup-variable-value exp env))
        ((quoted? exp) 
         (text-of-quotation exp))
        ((assignment? exp) 
         (eval-assignment exp env))
        ((definition? exp) 
         (eval-definition exp env))
        ((if? exp) 
         (eval-if exp env))
        ((lambda? exp)
         (make-procedure 
          (lambda-parameters exp)
          (lambda-body exp)
          env))
        ((begin? exp)
         (eval-sequence 
          (begin-actions exp) 
          env))
        ((cond? exp) 
         (eval (cond->if exp) env))
        ((let? exp)
         (eval-let exp env))
        ((application? exp)
         (mapply (eval (operator exp) env)
                (list-of-values 
                 (operands exp) 
                 env)))
        (else
         (error "Unknown expression 
                 type: EVAL" exp))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (mapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters
            procedure)
           arguments
           (procedure-environment
            procedure))))
        (else
         (error "Unknown procedure type: MAPPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((left (eval (first-operand exps) env))
             (right (list-of-values
                     (rest-operands exps)
                     env)))
        (cons left right))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;syntax for and and or (shared)
(define (exp-clauses exp)
  (cdr exp))
(define (empty-clauses? clauses)
  (null? clauses))
(define (first-clause clauses)
  (car clauses))
(define (rest-clauses clauses)
  (cdr clauses))
(define (last-clause clauses)
  (null? (cdr clauses)))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (and-iter clauses last-exp)
    (cond ((empty-clauses? clauses) last-exp)
          (else (let ((eval-first (eval (first-clause clauses) env)))
                  (if (false? eval-first) 'false
                      (and-iter (rest-clauses clauses) eval-first))))))
  (and-iter (exp-clauses exp) 'true))

(define (eval-or exp env)
  (define (or-iter clauses)
    (cond ((empty-clauses? clauses) 'false)
          (else (let ((eval-first (eval (first-clause clauses) env)))
                  (if (true? eval-first) eval-first
                      (or-iter (rest-clauses clauses)))))))
  (or-iter (exp-clauses exp)))


            
(define (eval-lambda exp env)
  (make-procedure
   (lambda-parameters exp)
   (lambda-body exp)
   env))

(define (eval-quoted exp env)
  (text-of-quotation exp))
(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps)
                        env))))

(define (eval-assignment exp env)
  (set-variable-value!
   (assignment-variable exp)
   (eval (assignment-value exp) env)
   env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

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
       (cdadr exp)
       (cddr exp))))

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

(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

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

(define (arrow-clause? clause)
  (and (list? clause)
       (> (length clause) 2)
       (eq? (cadr clause) '=>)))
(define (arrow-test arrow-clause)
  (car arrow-clause))
(define (arrow-recipient arrow-clause)
  (caddr arrow-clause))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (if (arrow-clause? first)
                (make-if (arrow-test first)
                         (list (arrow-recipient first) (arrow-test first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (let-clauses let)
  (cadr let))
(define (let-body let)
  (cddr let))
(define (let-vars let)
  (map car (let-clauses let)))
(define (let-exps let)
  (map cadr (let-clauses let)))
;let-body is still a little weird here (in terms of handling multiple expressions)
(define (make-let let-clauses . let-body)
  (cons 'let (cons let-clauses let-body)))
(define (first-let let-clauses)
  (car let-clauses))
(define (rest-lets let-clauses)
  (cdr let-clauses))


(define (named-let? let)
  (variable? (cadr let)))
(define (named-let-var let)
  (cadr let))
(define (named-let-bindings let)
  (caddr let))
(define (named-let-params let)
  (map car (named-let-bindings let)))
(define (named-let-exps let)
  (map cadr (named-let-bindings let)))
(define (named-let-body let)
  (cdddr let))
(define (make-named-let-definition let)
  (cons 'define
        (cons
         (cons (named-let-var let) (named-let-params let))
         (named-let-body let))))

(define (let->combination let)
  (if (named-let? let)
      (cons (make-lambda
             (named-let-params let)
             (cons (make-named-let-definition let)
                   (named-let-body let)))
            (named-let-exps let))
      (cons
       (make-lambda (let-vars let)
                    (let-body let))
       (let-exps let))))

(define (let*->nested-lets let*)
  (define (let*-iter let-clauses let-body)
    (if (null? let-clauses) (sequence->exp let-body)
        (make-let
         (list (first-let let-clauses))
         (let*-iter (rest-lets let-clauses) let-body))))
  (let*-iter (let-clauses let*) (let-body let*)))


(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(define (eval-for exp env)
  (eval (for->lambda exp) env))

(define (for-cond for)
  (let ((condition (cadr for)))
    (if (and (= (length condition) 3)
             (eq? (cadr condition) 'in))
        condition
        (error "For condition not recognized:" condition))))

(define (for-body for)
  (cddr for))

(define (for-cond-var for)
  (car (for-cond for)))

(define (for-cond-list for)
  (caddr (for-cond for)))

(define (for->lambda exp)
  (list (make-lambda
         '()
         (list (list
                'define
                '(iter list)
                (list 'if
                      (list 'not
                            (list 'null?
                                  'list))
                      (make-let
                       (list (list (for-cond-var exp) '(car list)))
                       (sequence->exp (for-body exp))
                       '(iter (cdr list)))
                      ''ok))
               (list 'iter (for-cond-list exp))))))

(define (while-cond while)
  (cadr while))
(define (while-body while)
  (cddr while))
(define (eval-while exp env)
  (eval (while->lambda exp) env))
(define (make-while cond body)
  (cons 'while
        (cons cond body)))

(define (until-cond until)
  (cadr until))
(define (until-body until)
  (cddr until))

(define (eval-until exp env)
  (eval (make-while (list 'not (until-cond exp))
                    (until-body exp)) env))

(define (do-cond do)
  (cadr do))
(define (do-body do)
  (cddr do))
(define (eval-do exp env)
  (eval (list 'begin
              (sequence->exp (do-body exp))
              (make-while (do-cond exp)
                          (do-body exp))) env))

(define (while->lambda exp)
  (list (make-lambda
         '()
         (list (list 'define
                     '(iter)
                     (make-if
                      (while-cond exp)
                      (list 'begin
                            (sequence->exp (while-body exp))
                            '(iter))
                      ''ok))
               '(iter)))))

(define (unbinding-value exp)
  (cadr exp))

(define (eval-unbinding exp env)
  (unbind-variable! (unbinding-value exp) env)
  'ok)

(define (make-letrec clauses body)
  (cons 'letrec (cons clauses body)))
(define (letrec-clauses exp)
  (cadr exp))
(define (letrec-body exp)
  (cddr exp))
(define (letrec-vars exp)
  (map car (letrec-clauses exp)))
(define (letrec-exps exp)
  (map cadr (letrec-clauses exp)))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))

(define (letrec->let exp)
  (cond
    ((null? (letrec-clauses exp)) (sequence->exp (letrec-body exp)))
    (else
     (apply make-let
            (cons
             (map
              (lambda (var) (list var ''*unassigned*))
              (letrec-vars exp))
             (append
              (map
               (lambda (var exp) (make-set var exp))
               (letrec-vars exp) (letrec-exps exp))
              (letrec-body exp)))))))
          
    
  




(define (true? x)
  (not (eq? x 'false)))

(define (false? x)
  (eq? x 'false))

(define (make-set set-variable set-body)
  (list 'set! set-variable set-body))

(define (scan-out-defines body)
  (define (scan-iter body let-clauses let-body)
    (cond ((null? body)
           (if (null? let-clauses)
               (reverse let-body)
               (list (cons 'let (cons (reverse let-clauses) (reverse let-body))))))               
          ((definition? (car body))
           (let ((def-var (definition-variable (car body)))
                 (def-val (definition-value (car body))))
             (scan-iter (cdr body)
                        (cons
                         (list def-var ''*unassigned*)
                         let-clauses)
                        (cons
                         (make-set def-var def-val)
                         let-body))))
          (else (scan-iter (cdr body) let-clauses (cons (car body) let-body)))))
  (scan-iter body '() '()))
                      
(define (make-procedure parameters body env)
    (list 'procedure parameters (scan-out-defines body) env))

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
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (apply-to-env empty-proc env-proc env)
  (if (eq? env the-empty-environment)
      (empty-proc)
      (env-proc env)))

(define (make-scanner var null-proc eq-proc)
  (lambda (env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars)
               (null-proc vars vals frame))
              ((eq? var (car vars))
               (eq-proc vars vals frame))
              (else (scan (cdr vars)
                          (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame)))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (apply-to-env
     (lambda () (error "Unbound variable" var))
     (make-scanner
      var
      (lambda (vars vals frame) (env-loop
                                 (enclosing-environment env)))
      (lambda (vars vals frame) (if (eq? (car vals) '*unassigned*)
                                    (error "Attempted to lookup an *unassigned* variable!")
                                    (car vals)
                                    )))
     env))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (apply-to-env
     (lambda () (error "Unbound variable: SET!" var))
     (make-scanner
      var
      (lambda (vars vals frame) (env-loop
                                 (enclosing-environment env)))
      (lambda (vars vals frame) (set-car! vals val)))
     env))
  (env-loop env))


(define (unbind-variable! var env)
  (define (env-loop env)
    (apply-to-env
     (lambda () (error "Can't unbind, not bound!" var))
     (make-scanner
      var
      (lambda (vars vals frame) (env-loop
                                 (enclosing-environment env)))
      (lambda (vars vals frame)
        (set-car! vars (cdr vars))
        (set-car! vals (cdr vals))))
     env))
  (env-loop env))
      


(define (define-variable! var val env)
  ((make-scanner
   var
   (lambda (vars vals frame) (add-binding-to-frame! var val frame))
   (lambda (vars vals frame) (set-car! vals val)))
   env))



(define (boolConvert oldBool)
  (cond ((eq? oldBool #f) 'false)
        ((eq? oldBool #t) 'true)
        (else "Invalid input old boolean:" oldBool)))

(define (convertBoolProc proc)
  (define (newBoolProc . terms)
    (let ((oldBool (apply proc terms)))
      (boolConvert oldBool)))
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



(define primitive-procedures
  (list (list 'car car)
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
        (list '> new>)))

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


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc)
  (cadr proc))



(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (eval input
                 the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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
