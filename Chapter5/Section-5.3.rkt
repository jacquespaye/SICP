#lang sicp

(#%require "Section-5.2.rkt")

;--------------------------------------------------------------
;Section 5.3, Exercise 5.21


(define leaves-recursive
  (make-machine
   `((cons,cons)
     (car,car)
     (cdr,cdr)
     (not-pair?,(lambda (x) (not (pair? x))))
     (+,+)
     (print,display)
     (read,read)
     (null?,null?))
   
   '(init
     (assign continue (label leaves-done))

     count-loop
     (test (op null?) (reg root))
     (branch (label null-tree))
     (test (op not-pair?) (reg root))
     (branch (label not-pair))
     
     (save continue)
     (save root)
     (assign continue (label after-car-count))
     (goto (label car-count))

     car-count
     (assign root (op car) (reg root))
     (goto (label count-loop))

     after-car-count ;val contains the car-count, continue contains after-car-count
     (restore root)
     (save root)
     (save val)
     (assign continue (label after-cdr-count))
     (goto (label cdr-count))

     cdr-count
     (assign root (op cdr) (reg root))
     (goto (label count-loop))

     after-cdr-count; val contains the cdr count, the car count is saved on the stack
     (assign cdr-count (reg val))
     (restore val)
     (assign val (op +) (reg cdr-count) (reg val))
     (restore root)
     (restore continue)
     (goto (reg continue))

     null-tree
     (assign val (const 0))
     (goto (reg continue))

     not-pair
     (assign val (const 1))
     (goto (reg continue))
     
     leaves-done
     (perform (op print) (reg val)))))

(define leaves-recursive-counter
  (make-machine
   `((cons,cons)
     (car,car)
     (cdr,cdr)
     (not-pair?,(lambda (x) (not (pair? x))))
     (+,+)
     (print,display)
     (read,read)
     (null?,null?))
   '(init
     (assign n (const 0))
     (assign continue (label leaves-done))
     
     count-loop
     (test (op null?) (reg root))
     (branch (label null-root))
     (test (op not-pair?) (reg root))
     (branch (label not-pair))

     (save root)
     (save continue)
     (assign continue (label after-car-count))
     (assign root (op car) (reg root))
     (goto (label count-loop))

     after-car-count
     (restore continue)
     (restore root)
     (assign root (op cdr) (reg root))
     (goto (label count-loop))

     not-pair
     (assign n (op +) (reg n) (const 1))
     (goto (reg continue))

     null-root
     (goto (reg continue))

     leaves-done
     (perform (op print) (reg n)))))



;--------------------------------------------------------------
;Section 5.3, Exercise 5.22

(define append-machine-1
  (make-machine
  `((cons, cons)
    (car,car)
    (cdr,cdr)
    (print,display)
    (null?, null?))
  '(init
    (assign continue (label append-done))

    append-loop
    (test (op null?) (reg x))
    (branch (label x-null))

    (assign car-x (op car) (reg x))
    (save car-x)
    (save continue)
    (assign continue (label after-cdr-append))
    (goto (label cdr-append))

    cdr-append
    (assign x (op cdr) (reg x))
    (goto (label append-loop))

    after-cdr-append
    (restore continue)
    (restore car-x)
    (assign result (op cons) (reg car-x) (reg result))
    (goto (reg continue))

    x-null
    (assign result (reg y))
    (goto (reg continue))

    append-done
    (perform (op print) (reg result)))))

(define append-machine-2
  (make-machine
  `((cons, cons)
    (car,car)
    (cdr,cdr)
    (print,display)
    (null?, null?)
    (set-cdr!, set-cdr!))

  '(init
    (assign continue (label append-done))
    (assign x-pointer (reg x))

    last-pair-loop
    (assign cdr-item (op cdr) (reg x-pointer))
    (test (op null?) (reg cdr-item))
    (branch (label last-found))
    (assign x-pointer (op cdr) (reg x-pointer))
    (goto (label last-pair-loop))

    last-found
    (perform (op set-cdr!) (reg x-pointer) (reg y))
    (goto (reg continue))

    append-done
    (perform (op print) (reg x)))))