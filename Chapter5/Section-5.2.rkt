#lang sicp
(#%provide (all-defined))

;--------------------------------------------------------------
;Section 5.1, Exercise 5.8


(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "Same label used twice:" next-inst)
                   (receive
                    insts
                    (cons
                     (make-label-entry
                      next-inst
                      insts)
                     labels)))
               (receive
                (cons (make-instruction
                       next-inst)
                      insts)
                labels)))))))
;--------------------------------------------------------------
;Section 5.1, Exercise 5.9


(define (make-operation-exp
         exp machine labels operations)
  (let ((op (lookup-prim 
             (operation-exp-op exp)
             operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "Attempt to use label as argument for operation:" e)
                    (make-primitive-exp 
                     e machine labels)))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))


;--------------------------------------------------------------
;Section 5.1, Exercise 5.11

(define (make-superstack-entry label stack)
  (list label stack))
(define (superstack-entry-label entry)
  (car entry))
(define (superstack-entry-stack entry)
  (cadr entry))
(define (make-superstack)
  (let ((stacks '()))
    (define (push push-pair)
      (let ((found (assoc (push-pair-name push-pair) stacks)))
        (if found
            (((superstack-entry-stack found) 'push) (push-pair-contents push-pair))
            (begin
              (let ((new-stack (make-stack)))
                ((new-stack 'push) (push-pair-contents push-pair))
                (set! stacks (cons (list (push-pair-name push-pair) new-stack) stacks)))))))
    (define (pop pop-name)
      (let ((found (assoc pop-name stacks)))
        (if found
            ((superstack-entry-stack found) 'pop)
            (error "Tried to pop from unknown stack:" pop-name))))
    (define (initialize)
      (map (lambda (stack-entry) ((superstack-entry-stack stack-entry) 'initialize)) stacks))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'initialize) (initialize))
            (else
             (error "Unknown request: SUPERSTACK:" message))))
    dispatch))



;--------------------------------------------------------------
;Section 5.1, Exercise 5.12

;Features  implemented in make-new-machine procedure below


;--------------------------------------------------------------
;Section 5.1, Exercise 5.13

(define (get-all-register-mentions controller-text)
  (cond ((null? controller-text) '())
        ((not (pair? controller-text)) '())
        ((register-exp? controller-text) (list controller-text))
        ((eq? (car controller-text) 'save) (list (list 'reg (cadr controller-text))))
        ((eq? (car controller-text) 'restore) (list (list 'reg (cadr controller-text))))
        ((eq? (car controller-text) 'assign) (append (list (list 'reg (assign-reg-name controller-text)))
                                                     (get-all-register-mentions (assign-value-exp controller-text))))
        ((pair? controller-text) (append (get-all-register-mentions (car controller-text))
                                         (get-all-register-mentions (cdr controller-text))))))

(define (get-all-registers controller-text)
  (unique-elements-only
   (get-all-register-mentions controller-text)))

(define (unique-elements-only the-list)
  (define (iter the-list result)
    (cond ((null? the-list) result)
          ((member? (car the-list) result) (iter (cdr the-list) result))
          (else (iter (cdr the-list) (cons (car the-list) result)))))
  (iter the-list '()))

;--------------------------------------------------------------
;Section 5.1, Exercise 5.15, 5.16, 5.17

;All features implemented in make-new-machine procedure below

;--------------------------------------------------------------
;Section 5.1, Exercise 5.18

;Feature implemented within make-register procedure below

;--------------------------------------------------------------
;Section 5.1, Exercise 5.18

;Feature implemented within make-new-machine below




;--------------------------------------------------------------
;Below find utility functions for this section




(define (tagged-list? item y)
  (eq? (car item) y))

(define (make-machine ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register)
                ((machine 'allocate-register)
                 (register-exp-reg register)))
              (get-all-registers controller-text))
    
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (toggle-trace-reg machine reg-name)
  ((machine 'toggle-trace-reg) reg-name))
(define (make-register name)
  (let ((contents '*unassigned*)
        (trace? #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace?
                   (begin
                     (display name) (display " <- ") (display value) (display " (was ") (display contents) (display ")") (display "\n")))
               (set! contents value)))
            ((eq? message 'toggle-trace)
             (if trace?
                 (set! trace? #f)
                 (set! trace? #t)))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))


(define (make-push-pair name contents)
  (cons name contents))
(define (push-pair-name pair)
  (car pair))
(define (push-pair-contents pair)
  (cdr pair))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth 
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)

    (define (print-statistics)
      
      (display (list 'total-pushes 
                     '= 
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth))
      (newline))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))
(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

(define (remove-list-if-singleton the-list)
  (cond ((null? the-list) the-list)
        ((null? (cdr the-list)) (car the-list))
        (else the-list)))

(define (make-breakpoint-counter adj fixed label)
  (list adj fixed label))
(define (breakpoint-count break-entry)
  (cadr break-entry))
(define (break-counter-adj break-counter)
  (car break-counter))
(define (break-counter-fixed break-counter)
  (cadr break-counter))
(define (break-counter-label break-counter)
  (caddr break-counter))


(define (remove-equal-items item list)
  (cond ((null? list) '())
        ((equal? item (car list)) (cdr list))
        (else (cons (car list) (remove-equal-items item (cdr list))))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-instruction-bank '())
        (the-entry-registers '())
        (the-saved-restored-registers '())
        (the-assignment-bank '())
        (the-instruction-counter 0)
        (trace-flag #f)
        (the-labels '*unassigned)
        (the-breakpoints '())
        (break-counter #f))
        
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () 
                         (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () 
                         (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error
             "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val
               (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        
        (let ((insts (get-contents pc)))
          (define (execute-and-continue)
            (if trace-flag
                (let ((label-entry (assq (instruction-text (car insts)) the-labels)))
                  (if label-entry
                      (begin
                        (display "at label: ")
                        (display (cadr label-entry))
                        (newline)))
                  (display "trace: ")
                  (display (instruction-text (car insts)))
                  (newline)))
                
            (set! the-instruction-counter (+ the-instruction-counter 1))
            ((instruction-execution-proc
              (car insts)))
            (execute))
          (if (null? insts)
              'done
              (begin


                (let ((label-entry (assq (instruction-text (car insts)) the-labels)))
                  (if (not (eq?  break-counter 'executed))
                      (if label-entry
                          (let ((breakpoint (assq (cadr label-entry) the-breakpoints)))
                            
                            (if breakpoint
                                (begin
                                       (set! break-counter (make-breakpoint-counter (breakpoint-count breakpoint) (breakpoint-count breakpoint)
                                                                                    (cadr label-entry)))))))
                      (set! break-counter #f)))

                
                (if break-counter
                    (begin
                      (set! break-counter (make-breakpoint-counter (- (break-counter-adj break-counter) 1) (break-counter-fixed break-counter)
                                                                   (break-counter-label break-counter)))
                      (cond ((eq? (break-counter-adj break-counter) 0) (begin 
                                                                              (display "Breaking execution on label ")
                                                                              (display (break-counter-label break-counter))
                                                                              (display " with original counter ")
                                                                              (display (break-counter-fixed break-counter))
                                                                              (display "\n")
                                                                              (set! break-counter 'executed)))
                            (else (execute-and-continue))))
                    (execute-and-continue))
                ))))
      (define (toggle-trace)
        (if trace-flag
            (set! trace-flag #f)
            (set! trace-flag #t)))
      (define (add-saved-restored item)
        (if (not (member? item the-saved-restored-registers))
            (set! the-saved-restored-registers (cons item the-saved-restored-registers))))
      (define (add-entry-point item)
        (if (not (member? item the-entry-registers))
            (set! the-entry-registers (cons item the-entry-registers))))
      (define (add-instruction-if-new instruction)
        (let ((inst-list (assoc (car instruction) the-instruction-bank)))
              (if inst-list
                  (if (not (member? instruction inst-list)) (set-cdr! inst-list (cons instruction (cdr inst-list))))
                  (set! the-instruction-bank (cons (list (car instruction) instruction)
                                                   the-instruction-bank)))))
      (define (add-assignment-source-if-new assignment)
        (let* ((reg-name (assign-reg-name assignment))
               (source (remove-list-if-singleton (assign-value-exp assignment)))
               (assign-list (assoc reg-name the-assignment-bank)))
          (if assign-list
              (if (not (member? source assign-list)) (set-cdr! assign-list (cons source (cdr assign-list))))
              (set! the-assignment-bank (cons (list reg-name source) the-assignment-bank)))))
      (define (extract-first-instruction raw-label)
        (if (null? (cdr raw-label)) '()
            (caadr raw-label)))
      (define (store-labels labels)
        (set! the-labels
              (map (lambda (raw-label)
                     (list
                      (extract-first-instruction raw-label)
                      (car raw-label))) labels)))
      (define (toggle-trace-reg name-reg)
        (let ((chosen-register (lookup-register name-reg)))
          (chosen-register 'toggle-trace)))

      (define (add-break-entry label count)
        (set! the-breakpoints (cons (list label count) the-breakpoints)))
      (define (remove-break-point label count)
        (set! the-breakpoints (remove-equal-items (list label count) the-breakpoints)))
      (define (remove-all-break-points)
        (set! the-breakpoints '()))

      
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents!
                pc
                the-instruction-sequence)
               (execute))
              ((eq? message
                    'install-instruction-sequence)
               (lambda (seq)
                 (set!
                  the-instruction-sequence
                  seq)))
              ((eq? message 'register-table) register-table)
              ((eq? message
                    'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message
                    'install-operations)
               (lambda (ops)
                 (set! the-ops
                       (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations)
               the-ops)
              ((eq? message 'add-instruction-to-bank)
               add-instruction-if-new)
              ((eq? message 'instruction-bank) the-instruction-bank)
              ((eq? message 'add-entry-point)
               add-entry-point)
              ((eq? message 'entry-registers) the-entry-registers)
              ((eq? message 'add-saved-restored)
               add-saved-restored)
              ((eq? message 'saved-restored) the-saved-restored-registers)
              ((eq? message 'add-assign-source) add-assignment-source-if-new)
              ((eq? message 'assign-sources) the-assignment-bank)
              ((eq? message 'instruction-count) the-instruction-counter)
              ((eq? message 'reset-inst-count) (set! the-instruction-counter 0))
              ((eq? message 'toggle-trace) (toggle-trace))
              ((eq? message 'store-labels) store-labels)
              ((eq? message 'labels) the-labels)
              ((eq? message 'toggle-trace-reg) toggle-trace-reg)
              ((eq? message 'set-breakpoint) add-break-entry)
              ((eq? message 'proceed-execution) (execute))
              ((eq? message 'remove-breakpoint) remove-break-point)
              ((eq? message 'remove-all-breakpoints) (remove-all-break-points))
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (proceed-machine machine)
  (machine 'proceed-execution))
(define (cancel-breakpoint machine label n)
  ((machine 'remove-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'remove-all-breakpoints))

(define (start machine)
  (machine 'start))
(define (get-register-contents
         machine register-name)
        (get-contents
         (get-register machine register-name)))

(define (set-register-contents!
         machine register-name value)
  (set-contents!
   (get-register machine register-name)
   value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    ((machine 'store-labels) labels)
                    (update-insts! insts labels machine)
                    insts)))


(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       ((machine 'add-instruction-to-bank)
        (instruction-text inst))
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc!
         inst
         proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (make-execution-procedure 
         inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign 
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test 
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch 
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction 
                      type: ASSEMBLE"
                     inst))))


(define (make-assign 
         inst machine labels operations pc)
  (let ((target 
         (get-register 
          machine 
          (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    ((machine 'add-assign-source) inst)
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()   ; execution procedure
                   ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))



(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


(define 
  (make-test 
   inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition 
                machine
                labels
                operations)))
          (lambda () 
            (set-contents! 
             flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: 
                ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define 
  (make-branch 
   inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label 
                labels 
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: 
                ASSEMBLE"
               inst))))


(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label 
                   labels
                   (label-exp-label dest))))
             (lambda () 
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register 
                   machine
                   (register-exp-reg dest))))
             ((machine 'add-entry-point) (register-exp-reg dest))
             (lambda ()
               (set-contents! 
                pc
                (get-contents reg)))))
          (else (error "Bad GOTO instruction: 
                        ASSEMBLE"
                       inst)))))


(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register 
              machine
              (stack-inst-reg-name inst))))
    ((machine 'add-saved-restored) (stack-inst-reg-name inst))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    ((machine 'add-saved-restored) (stack-inst-reg-name inst))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))


(define (stack-inst-reg-name 
         stack-instruction)
  (cadr stack-instruction))

(define (make-perform 
         inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: 
                ASSEMBLE"
               inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label 
                 labels
                 (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register
                   machine
                   (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: 
                      ASSEMBLE"
                     exp))))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp) 
  (cadr exp))




(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (member? item the-list)
  (cond ((null? the-list) #f)
        ((equal? item (car the-list)) the-list)
        (else (member? item (cdr the-list)))))

(define (add-if-new item tagged-list)
  (if (not (member? item tagged-list))
      (set-cdr! tagged-list (cons item (cdr tagged-list)))))
