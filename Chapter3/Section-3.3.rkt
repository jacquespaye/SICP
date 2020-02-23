#lang sicp


;--------------------------------------------------------------
;Section 3.3, Exercise 3.17


(define (count-pairs-wrong x)
  (if (not (pair? x))
      0
      (+ (count-pairs-wrong (car x))
         (count-pairs-wrong (cdr x))
         1)))

(define (count-pairs x)
  (let ((pointerList '()))
    (define (count-iter x)
      (cond ((not (pair? x)) 0)
            ((memq x pointerList) 0)
            (else (set! pointerList (cons x pointerList))
                  (+ (count-iter (car x))
                     (count-iter (cdr x))
                     1)
                  )))
    (count-iter x)))


;--------------------------------------------------------------
;Section 3.3, Exercise 3.18
(define (cycle? x)
  (let ((pointerList '()))
    (define (cycleIter x)
      (cond ((memq x pointerList) #t)
            ((not (pair? x)) #f)
            (else
             (set! pointerList (cons x pointerList))
             (cycleIter (cdr x)))))
    (cycleIter x)))

(define (constant-cycle? x)
  (let ((tortoise x)
        (hare (cdr x)))
    (define (iter tortoise hare)
      (cond ((not (pair? hare)) #f)
            ((not (pair? (cdr hare))) #f)
            ((eq? tortoise hare) #t)
            (else (iter (cdr tortoise) (cddr hare)))))
    (iter tortoise hare)))



;--------------------------------------------------------------
;Section 3.3, Exercise 3.21
(define (print-queue queue)
  (display (front-ptr queue)))



;--------------------------------------------------------------
;Section 3.3, Exercise 3.22

(define (make-queue-object)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (print)
      (display front-ptr)
      (newline))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "Front called with empty queue" front-ptr)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (print))
              (else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)
                    (print)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else (set! front-ptr (cdr front-ptr))
                  (print))))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else (error "Unknown action--MAKE-QUEUE" m))))
    dispatch))



;--------------------------------------------------------------
;Section 3.3, Exercise 3.23

(define (empty-deque? deque)
  (or (null? (front-ptr-deque deque))
      (null? (rear-ptr-deque deque))))

(define (print-deque deque)
  (define (print-iter item)
    (cond
      ((eq? item (rear-ptr deque))
       (display (this-element item))
       (display ")"))
      (else (display (this-element item))
            (display " ")
            (print-iter (next-element item)))))
  (cond
    ((empty-deque? deque) (display "()"))
    (else
     (display "(")
     (print-iter (front-ptr deque)))))
    
          
(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((new-item (make-element item '() '())))
           (set-front-deque-ptr! deque new-item)
           (set-rear-deque-ptr! deque new-item))
         (print-deque deque))
        (else
         (set-front-deque-ptr!
          deque
          (make-element item '() (front-ptr-deque deque)))
         (print-deque deque))))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((new-item (make-element item '() '())))
           (set-rear-deque-ptr! deque new-item)
           (set-front-deque-ptr! deque new-item))
         (print-deque deque))
        (else
         (let ((new-item (make-element item (rear-ptr-deque deque) '())))
           (set-next! (rear-ptr-deque deque) new-item)
           (set-rear-deque-ptr! deque new-item))
         (print-deque deque))))


(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "Tried to front delete from an empty deque" deque))
        ((eq? (front-ptr-deque deque)
              (rear-ptr-deque deque))
         (set-front-deque-ptr! deque '())
         (set-rear-deque-ptr! deque '())
         (print-deque deque))
        (else
         (set-front-deque-ptr! deque (next-element (front-ptr-deque deque)))
         (print-deque deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "Tried to rear delete from an empty deque" deque))
        ((eq? (front-ptr-deque deque)
              (rear-ptr-deque deque))
         (set-front-deque-ptr! deque '())
         (set-rear-deque-ptr! deque '())
         (print-deque deque))
        (else
         (set-rear-deque-ptr! deque (last-element (rear-ptr-deque deque)))
         (print-deque deque))))

(define (front-deque deque)
  (car (front-ptr-deque deque)))

(define (rear-deque deque)
  (car (rear-ptr-deque deque)))
         


(define (make-deque)
  (cons '() '()))

(define (front-ptr-deque deque)
  (car deque))

(define (rear-ptr-deque deque)
  (cdr deque))

(define (set-front-deque-ptr! deque item)
  (set-car! deque item))

(define (set-rear-deque-ptr! deque item)
  (set-cdr! deque item))



;--------------------------------------------------------------
;Section 3.3, Exercise 3.24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else  (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr!
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr!
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))



;--------------------------------------------------------------
;Section 3.3, Exercise 3.25

(define (make-multi-table)
  (let ((local-table (list '*table*)))
    (define (first-non-pair list)
      (cond ((null? list) #f)
            ((not (pair? (car list))) list)
            (else (first-non-pair  (cdr list)))))
    (define (fetch-value list)
      (let ((partial-list (first-non-pair list)))
        (if partial-list (car partial-list) #f)))
    (define (insert-value level value)
      (let ((existing-value (first-non-pair (cdr level))))
        (if existing-value
            (set-car! existing-value value)
            (set-cdr! level (cons value (cdr level))))))
    (define (make-level key level)
      (set-cdr! level (cons (list key) (cdr level)))
      (cadr level))
    (define (lookup keys)
      (define (lookup-iter keys level)
        (cond
          ((not level) #f)
          ((null? keys) (fetch-value (cdr level)))
          (else (lookup-iter (cdr keys) (assoc (car keys) (cdr level))))))
      (lookup-iter keys local-table))
    (define (insert! keys value)
      (define (insert-iter! keys value level)
          (cond
            ((null? keys) (insert-value level value))
            ((assoc (car keys) (cdr level)) (insert-iter! (cdr keys) value (assoc (car keys) (cdr level))))
            (else (insert-iter! (cdr keys) value (make-level (car keys) level)))))
      (insert-iter! keys value local-table))
    (define (dispatch m)
      (cond
        ((eq? m 'insert-proc) insert!)
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'table) local-table)))
    dispatch))


;--------------------------------------------------------------
;Section 3.3, Exercise 3.26

(define (make-binary-table)
  (let ((local-table (list '*table*)))
    (define (insert! key value)
      (define (insert-iter! key value current-node)
        (cond
          ((= key (get-key current-node)) (set-value! current-node value))
          ((< key (get-key current-node)) (if (null? (left current-node))
                                          (set-left! current-node (make-node key value '() '()))
                                          (insert-iter! key value (left current-node))))
          ((> key (get-key current-node)) (if (null? (right current-node))
                                          (set-right! current-node (make-node key value '() '()))
                                          (insert-iter! key value (right current-node))))))
      (if (null? (cdr local-table))
          (set-cdr! local-table (make-node key value '() '()))
          (insert-iter! key value (cdr local-table))))
    (define (lookup key)
      (define (lookup-iter key current-node)
        (cond ((null? current-node) #f)
              ((= key (get-key current-node)) (get-value current-node))
              ((< key (get-key current-node)) (lookup-iter key (left current-node)))
              ((> key (get-key current-node)) (lookup-iter key (right current-node)))))
      (lookup-iter key (cdr local-table)))
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        ((eq? m 'table) local-table)
        (else (error "Unknown request:" m))))
    dispatch))



;--------------------------------------------------------------
;Section 3.3, Exercise 3.27



(define (fib n)
  (cond  ((= n 0) 0)
         ((= n 1) 1)
         (else (+ (fib (- n 1))
                  (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2))))))))





;--------------------------------------------------------------
;Utility functions for this section:

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)










(define (new-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation: CONS" m))))
  dispatch)

(define (new-car z) (z 'car))
(define (new-cdr z) (z 'cdr))

(define (new-set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (new-set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)


(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "Front called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue)
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr!
               queue
               (cdr (front-ptr queue)))
              queue)))



;object definitions of queue

(define (empty-queue?-object queue)
  (queue 'empty-queue?))

(define (front-queue-object queue)
  (queue 'front-queue))

(define (insert-queue!-object queue item)
  ((queue 'insert-queue!) item)
  queue)

(define (delete-queue!-object queue)
  (queue 'delete-queue!)
  queue)



;define "element" data structure to use as the items in the list

(define (make-element item last next)
  (cons item (cons last next)))
(define (last-element element)
  (cadr element))
(define (next-element element)
  (cddr element))
(define (this-element element)
  (car element))
(define (set-next! element item)
  (set-cdr! (cdr element) item))
(define (set-last! element item)
  (set-car! (cdr element) item))


(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((not (pair? (car records))) (assoc key (cdr records)))
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr!
               subtable
               (cons (cons key-2 value)
                     (cdr subtable)))))
        (set-cdr!
         table
         (cons (list key-1 (cons key-2 value))
               (cdr table)))))
  'done)

(define (tolerant? a b)
  (= (round a) b))








(define (make-node key value left right)
  (list key value left right))

(define (get-key node)
  (car node))

(define (get-value node)
  (cadr node))

(define (left node)
  (caddr node))

(define  (right node)
  (cadddr node))

(define (set-value! node value)
  (set-car! (cdr node) value))

(define (set-left! node new-node)
  (set-car! (cddr node) new-node))

(define (set-right! node new-node)
  (set-car! (cdddr node) new-node))



