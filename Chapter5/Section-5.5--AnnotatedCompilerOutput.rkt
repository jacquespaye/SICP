#lang sicp


;--------------------------------------------------------------
;Section 5.4, Exercise 5.34
;Annotated compiled code is below



;ITERATIVE:
((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry18) (reg env));Top-level procedure definition
  (goto (label after-lambda19))
  entry18
  (assign env (op compiled-procedure-env) (reg proc));Here we are setting up the top-level procedure environment
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env));Add n as variable
  (assign val (op make-compiled-procedure) (label entry20) (reg env));we define the internal procedure
  (goto (label after-lambda21))
  entry20
  (assign env (op compiled-procedure-env) (reg proc));set up internal proc environment
  (assign
   env
   (op extend-environment)
   (const (product counter))
   (reg argl)
   (reg env));we add product and counter to the environment
  (save continue);SAVE continue
  (save env);SAVE env
  (assign proc (op lookup-variable-value) (const >) (reg env));Fetch >
  (assign val (op lookup-variable-value) (const n) (reg env));fetch n
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env));fetch counter
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc));this will be true
  (branch (label primitive-branch25))
  compiled-branch26
  (assign continue (label after-call27))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch25;this will execute
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call27
  (restore env);RESTORE env
  (restore continue);RESTORE continue
  (test (op false?) (reg val))
  (branch (label false-branch23))
  true-branch22
  (assign val (op lookup-variable-value) (const product) (reg env));if true, we are done!
  (goto (reg continue))
  false-branch23
  (assign proc (op lookup-variable-value) (const iter) (reg env));fetch the procedure (itself)
  (save continue);SAVE continue
  (save proc);SAVE proc
  (save env);SAVE env
  (assign proc (op lookup-variable-value) (const +) (reg env));We are now evaluating (+ counter 1)
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc));this will be true
  (branch (label primitive-branch31))
  compiled-branch32
  (assign continue (label after-call33))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch31
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call33
  (assign argl (op list) (reg val));we now have (+ counter 1), we need to evaluate (* counter product)
  (restore env);RESTORE env
  (save argl);SAVE argl
  (assign proc (op lookup-variable-value) (const *) (reg env)) ;fetch *
  (assign val (op lookup-variable-value) (const product) (reg env));fetch product
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env));fetch counter
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc)); this will be true
  (branch (label primitive-branch28))
  compiled-branch29
  (assign continue (label after-call30))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch28;this will execute
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call30
  (restore argl);RESTORE argl
  (assign argl (op cons) (reg val) (reg argl));assemble argl
  (restore proc);RESTORE proc--restore iter
  (restore continue);RESTORE continue now everything is off the stack
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch34))
  compiled-branch35
  (assign val (op compiled-procedure-entry) (reg proc));we are now going to go directly into the next "iter" (did not save anything on stack)
  (goto (reg val))
  primitive-branch34
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call36
  after-if24
  after-lambda21
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1));here we will execute the actual (iter 1 1) operation)
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch37))
  compiled-branch38
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch37
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call39
  after-lambda19
  (perform (op define-variable!) (const factorial) (reg val) (reg env));here we actually define the outer "factorial" procedure
  (assign val (const ok))))

;RECURSIVE
((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry40) (reg env))
  (goto (label after-lambda41))
  entry40
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue);SAVE continue
  (save env);SAVE env
  (assign proc (op lookup-variable-value) (const =) (reg env));we are setting up for the test
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch45))
  compiled-branch46
  (assign continue (label after-call47))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch45
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call47
  (restore env);RESTORE env
  (restore continue);RESTORE continue
  (test (op false?) (reg val));testing if this is the last iteration
  (branch (label false-branch43))
  true-branch42
  (assign val (const 1));we have reached the end of the recursion, we dive back to continue (and begin the lengthy restoration  process)
  (goto (reg continue))
  false-branch43
  (assign proc (op lookup-variable-value) (const *) (reg env));we need to set up a recursion, we will save * on the stack
  (save continue);SAVE continue (this is the top-level continue initially)
  (save proc);SAVE proc
  (assign val (op lookup-variable-value) (const n) (reg env));we fetch n
  (assign argl (op list) (reg val))
  (save argl);SAVE argl
  (assign proc (op lookup-variable-value) (const factorial) (reg env));get the factorial procedure
  (save proc);SAVE proc-->we save factorial on stack
  (assign proc (op lookup-variable-value) (const -) (reg env));fetch -
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch48))
  compiled-branch49
  (assign continue (label after-call50))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch48
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call50;we have now calc'd (- n 1)
  (assign argl (op list) (reg val))
  (restore proc);RESTORE proc
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch51))
  compiled-branch52
  (assign continue (label after-call53));this will execute, we start a new execution, a bunch is saved on the stack (continue,proc,argl)
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch51
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call53
  (restore argl);RESTORE argl -->we undo the reg saves
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc);RESTORE proc-->we undo the reg saves
  (restore continue);RESTORE continue-->we undo the reg saves
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch54))
  compiled-branch55
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch54
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue));we now have the answer (after a lot of save/restore
  after-call56
  after-if44
  after-lambda41
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))


;--------------------------------------------------------------
;Section 5.4, Exercise 5.35
;Annotated compiled code is below

(assign val (op make-compiled-procedure) 
            (label entry16) 
            (reg env));we are definining a procedure
  (goto (label after-lambda15))
entry16
  (assign env (op compiled-procedure-env);we begin by initializing proc environment
              (reg proc))
  (assign env (op extend-environment) ;we have one variable, x
              (const (x)) 
              (reg argl) 
              (reg env))
  (assign proc (op lookup-variable-value) 
               (const +) ;we lookup +
               (reg env))
  (save continue) (save proc) (save env);SAVE continue, SAVE proc, SAVE env
  (assign proc (op lookup-variable-value) 
               (const g) 
               (reg env));we lookup g, a procedure
  (save proc);save g on stack
  (assign proc (op lookup-variable-value) ;we lookup +
               (const +) 
               (reg env))
  (assign val (const 2));first argument in-->2
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value)
              (const x) 
              (reg env));second argument in ->x
  (assign argl (op cons)
               (reg val)
               (reg argl));arglist (x, 2)
  (test (op primitive-procedure?)
        (reg proc))
  (branch (label primitive-branch19));this will run, this is addition
compiled-branch18
  (assign continue (label after-call17))
  (assign val
          (op compiled-procedure-entry)
          (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val
          (op apply-primitive-procedure);val assigned (+ x 2)
          (reg proc) 
          (reg argl))
after-call17
  (assign argl (op list) (reg val));we are now making an argl, leftmost argument is (+ x 2)
  (restore proc);G is back in proc
  (test (op primitive-procedure?)
        (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20));this will run
  (assign val
          (op compiled-procedure-entry)
          (reg proc));val contains (G (+ x 2))
  (goto (reg val))
primitive-branch22
  (assign val 
          (op apply-primitive-procedure) 
          (reg proc) 
          (reg argl))
after-call20
  (assign argl (op list) (reg val));val contains (G (+ x 2)), this becomes an argl
  (restore env);env restored
  (assign val
          (op lookup-variable-value) 
          (const x) 
          (reg env));val is looked up, val=x
  (assign argl
          (op cons)
          (reg val)
          (reg argl));argl= (x (G (+ x 2))
  (restore proc);restore proc to +
  (restore continue);continue restored
  (test (op primitive-procedure?)
        (reg proc))
  (branch (label primitive-branch25));this will run, (+ x (g (+ x 2)))
compiled-branch24
  (assign val (op compiled-procedure-entry)
              (reg proc))
  (goto (reg val))
primitive-branch25
  (assign val 
          (op apply-primitive-procedure)
          (reg proc) 
          (reg argl))
  (goto (reg continue));we have reached the end, val= (+ x (G (+ x 2)))
after-call23
after-lambda15
  (perform (op define-variable!) 
           (const f) 
           (reg val) 
           (reg env));we are defining function f
  (assign val (const ok))


;We can guess that the original code compiled was:
;(define (f x)
 ; (+ x (g (+ x 2))))