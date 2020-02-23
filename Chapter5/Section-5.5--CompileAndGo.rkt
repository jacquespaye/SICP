#lang sicp

(#%require (prefix ds: "Section-5.2.rkt"))
(#%require "Section-5.4--MetacircularSource.rkt")
(#%require "Section-5.5.rkt")
(#%require "Section-5.5--MetacircularForCompilation.rkt")


;--------------------------------------------------------------
;Section 5.5, Exercise 5.48
;Compile-and-run is implemented below

;--------------------------------------------------------------
;Section 5.5, Exercise 5.49
;To create a RCEPL, simply replace eceval below with the following:

(define eceval-2
  (ds:make-machine
   eceval-operations
   '(compound-apply
     (assign compapp (reg compapp))
     (assign env (reg env))
     (assign proc (reg proc))
     (assign val (reg val))
     (assign argl (reg argl))
     (assign continue (reg continue));all these are here only for continuity w/ old compiler
     read-compile-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; Comp-Eval input:"))
     (assign exp (op read))
     (assign val (op compile-for-comp-and-run) (reg exp))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val))
     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output)
              (const ";;; Comp-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-compile-print-loop)))))


;--------------------------------------------------------------
;Section 5.5, Exercise 5.50
;To compile the metacircular evaluator, simply run  (compile-and-go the-evaluator)

(define fib-machine
  (ds:make-machine
   (list (list 'print display)
         (list 'read read)
         (list '+ +)
         (list '* *)
         (list '- -)
         (list '< <))
   '(init
     (perform (op initialize-stack))
     (assign n (op read))
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n − 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)           ; save old value of n
     (assign n 
             (op -)
             (reg n)
             (const 1)) ; clobber n to n-1
     (goto 
      (label fib-loop)) ; perform recursive call
     afterfib-n-1 ; upon return, val contains Fib(n − 1)
     (restore n)
     ;; set up to compute Fib(n − 2)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)         ; save Fib(n − 1)
     (goto (label fib-loop))
     afterfib-n-2 ; upon return, val contains Fib(n − 2)
     ;(restore n) ; we transfer the old value of n (fib(n-1)) into the n register so we can add it to the current val (which contains fib(n-2))
     (assign n 
             (reg val)) ; n now contains Fib(n − 2)
     (restore val)
     (restore continue)
     (assign val        ; Fib(n − 1) + Fib(n − 2)
             (op +) 
             (reg val)
             (reg n))
     (goto              ; return to caller,
      (reg continue))   ; answer is in val
     immediate-answer
     (assign val 
             (reg n))   ; base case: Fib(n) = n
     (goto (reg continue))
     fib-done
     (perform (op print) (reg val))
     (perform (op print) (const "\n"))
     (perform (op print-stack-statistics))
     (goto (label init)))))

(define fact-machine-rec
  (ds:make-machine
   (list
     (list 'read read)
     (list '= =)
     (list '- -)
     (list '* *)
     (list 'print display))
     
   '(init
     (perform (op initialize-stack))
     (assign n (op read))
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done
     (perform (op print) (reg val))
     (perform (op print) (const "\n"))
     (perform (op print-stack-statistics))
     (goto (label init)))))

(define (user-print-for-compiler object)
  (cond ((compound-procedure? object)
         (display
          (list 'compound-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))


(define start ds:start)

(define the-global-environment
  (setup-environment))

(define (get-global-environment)
  the-global-environment)

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops) (null? (cdr ops)))
(define (compile-for-comp-and-run expression)
  (let ((instructions
         (ds:assemble
          (statements
           (compile
            expression 'val 'return the-empty-comp-env))
          eceval)))
    instructions))

(define (comp-and-run? exp)
  (tagged-list? exp 'compile-and-run))
(define (comp-run-exp exp)
  (cadadr exp))

(define eceval-operations
  `((prompt-for-input,prompt-for-input)
    (announce-output,announce-output)
    (read,read)
    (get-global-environment,get-global-environment)
    (user-print,user-print)
    (self-evaluating?,self-evaluating?)
    (variable?,variable?)
    (quoted?,quoted?)
    (assignment?,assignment?)
    (definition?,definition?)
    (if?,if?)
    (lambda?,lambda?)
    (begin?,begin?)
    (application?,application?)
    (lookup-variable-value,lookup-variable-value)
    (text-of-quotation,text-of-quotation)
    (lambda-parameters,lambda-parameters)
    (lambda-body,lambda-body)
    (make-procedure,make-procedure)
    (operands,operands)
    (operator,operator)
    (empty-arglist,empty-arglist)
    (no-operands?,no-operands?)
    (first-operand,first-operand)
    (last-operand?,last-operand?)
    (adjoin-arg,adjoin-arg)
    (rest-operands,rest-operands)
    (primitive-procedure?,primitive-procedure?)
    (compound-procedure?,compound-procedure?)
    (apply-primitive-procedure,apply-primitive-procedure)
    (procedure-parameters,procedure-parameters)
    (procedure-environment,procedure-environment)
    (extend-environment,extend-environment)
    (procedure-body,procedure-body)
    (begin-actions,begin-actions)
    (first-exp,first-exp)
    (last-exp?,last-exp?)
    (rest-exps,rest-exps)
    (if-predicate,if-predicate)
    (true?,true?)
    (if-alternative,if-alternative)
    (if-consequent,if-consequent)
    (assignment-variable,assignment-variable)
    (assignment-value,assignment-value)
    (set-variable-value!,set-variable-value!)
    (definition-variable,definition-variable)
    (definition-value,definition-value)
    (define-variable!,define-variable!)
    (cond?,cond?)
    (cond->if,cond->if)
    (let?,let?)
    (let->combination,let->combination)
    (let*?,let*?)
    (let*->nested-lets,let*->nested-lets)
    (cond-clauses,cond-clauses)
    (null?,null?)
    (car,car)
    (cond-predicate,cond-predicate)
    (cond-else-clause?,cond-else-clause?)
    (cdr,cdr)
    (cond-actions,cond-actions)
    (print,display)
    (compiled-procedure?,compiled-procedure?)
    (compiled-procedure-entry,compiled-procedure-entry)
    (make-compiled-procedure,make-compiled-procedure)
    (compiled-procedure-env,compiled-procedure-env)
    (false?,false?)
    (list,list)
    (cons,cons)
    (+,+)
    (-,-)
    (*,*)
    (=,new=)
    (lexical-address-lookup,lexical-address-lookup)
    (lexical-address-set!,lexical-address-set!)
    (compile-for-comp-and-run,compile-for-comp-and-run)
    (comp-and-run?,comp-and-run?)
    (comp-run-exp,comp-run-exp)))


(define eceval
  (ds:make-machine
   eceval-operations
   '(
     (assign compapp (label compound-apply))
     (branch (label external-entry))
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output)
              (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val))

     comp-and-run
     (assign exp (op comp-run-exp) (reg exp))
     (assign val (op compile-for-comp-and-run) (reg exp))
     (assign continue (label print-result))
     (goto (reg val))

     unknown-expression-type
     (assign 
      val
      (const unknown-expression-type-error))
     (goto (label signal-error))
     unknown-procedure-type
     ; clean up stack (from apply-dispatch):
     (restore continue)    
     (assign 
      val
      (const unknown-procedure-type-error))
     (goto (label signal-error))
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

  
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op cond?) (reg exp))
     (branch (label ev-cond-2))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op let*?) (reg exp))
     (branch (label ev-let*))
     (test (op comp-and-run?) (reg exp))
     (branch (label comp-and-run))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val
             (op lookup-variable-value)
             (reg exp)
             (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val
             (op text-of-quotation)
             (reg exp))
     (goto (reg continue))

     ev-cond-2
     ;null?
     (save continue) ;save continue for after
     (save env) ;save env so we can evaluate the consequent
     (assign unev (op cond-clauses) (reg exp))
     (goto (label ev-cond-clauses))

     ev-cond-clauses ;continue and env are saved on stack
     (test (op null?) (reg unev))
     (branch (label null-handler))
     (assign exp (op car) (reg unev)); put first clause in exp
     (test (op cond-else-clause?) (reg exp))
     (branch (label true-handler))
     (assign exp (op cond-predicate) (reg exp));put first predicate in exp
     ;(perform (op print) (reg unev))
     
     (restore env)
     (save env)
     (save unev)
     (assign continue (label after-pred-eval))
     (goto (label eval-dispatch))
     after-pred-eval
     (restore unev)
     (test (op true?) (reg val))
     (branch (label true-handler))
     (assign unev (op cdr) (reg unev));switch to next clause
     (goto (label ev-cond-clauses))

     null-handler
     (restore env)
     (restore continue)
     
     (assign val (const false))
     (goto (reg continue))

     true-handler
     (restore env)
     (restore continue)
     (assign unev (op car) (reg unev)); put first clause in exp
     (assign unev (op cond-actions) (reg unev));put first actions in exp
     (save continue)
     (goto (label ev-sequence))

     ev-cond
     (assign exp (op cond->if) (reg exp))
     (goto (label eval-dispatch))

     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))

     ev-let*
     (assign exp (op let*->nested-lets) (reg exp))
     (goto (label eval-dispatch))

     ev-lambda
     (assign unev
             (op lambda-parameters)
             (reg exp))
     (assign exp 
             (op lambda-body)
             (reg exp))
     (assign val 
             (op make-procedure)
             (reg unev)
             (reg exp)
             (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign
      continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)             ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))    ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ;ev-appl-make-thunk
     ;(assign exp (op list) (const 'thunk) (reg exp) (reg env))
     ;(goto (reg continue))

     ev-appl-operand-loop
     (save argl)
     (assign exp
             (op first-operand)
             (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue 
             (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl 
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (assign unev
             (op rest-operands)
             (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue 
             (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl 
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (test (op compiled-procedure?) (reg proc))
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))

     compiled-apply
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev 
             (op procedure-parameters)
             (reg proc))
     (assign env
             (op procedure-environment)
             (reg proc))
     (assign env
             (op extend-environment)
             (reg unev)
             (reg argl)
             (reg env))
     (assign unev
             (op procedure-body)
             (reg proc))
     (goto (label ev-sequence))

     ev-begin
     (assign unev
             (op begin-actions)
             (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue
             (label ev-sequence-continue))
     (goto (label eval-dispatch))
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev
             (op rest-exps)
             (reg unev))
     (goto (label ev-sequence))
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ev-if
     (save exp)   ; save expression for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     ; evaluate the predicate:
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev 
             (op assignment-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp
             (op assignment-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue
             (label ev-assignment-1))
     ; evaluate the assignment value:
     (goto (label eval-dispatch))  
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val
             (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev 
             (op definition-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp 
             (op definition-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     ; evaluate the definition value:
     (goto (label eval-dispatch))  
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val (const ok))
     (goto (reg continue)))))



(define (compile-and-go expression)
  (let ((instructions
         (ds:assemble 
          (statements
           (compile 
            expression 'val 'return the-empty-comp-env))
          eceval)))
    (set! the-global-environment
          (setup-environment))
    (ds:set-register-contents! 
     eceval 'val instructions)
    (ds:set-register-contents! 
     eceval 'flag true)
    (start eceval)))