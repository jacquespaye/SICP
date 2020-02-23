#lang sicp

;--------------------------------------------------------------
;Section 3.4, Exercise 3.47


(define (make-semaphore n)

  ;helper function used to create list of mutexes
  (define (make-nlong-list item count)
    (define (make-list-iter item count result)
      (cond ((eq? count 0) result)
            (else (make-list-iter item (- count 1) (cons (item) result)))))
    (make-list-iter item count '()))

  ;function to test-and-set mutexes until one is acquired
  (define (acquire-a-mutex mutex-list)
    (cond ((null? mutex-list) #f)
          ((not ((car mutex-list) 'test-and-set)) (car mutex-list))
          (else (acquire-a-mutex (cdr mutex-list)))))
  (let ((mutex-list (make-nlong-list make-mutex n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (let ((output (acquire-a-mutex mutex-list)))
               (if output
                   output
                   (the-semaphore 'acquire))))
            ((eq? m 'list)
             mutex-list)
            ((eq? m 'values)
             (map (lambda (x) (x 'cell)) mutex-list))
            ((eq? m 'test)
             (acquire-a-mutex mutex-list))
            (else (error "Don't recognize that input to semaphore"))))
    the-semaphore))


(define (make-semaphore-2 n)
  (define (test-decrement n) ;atomic
    (if (not (> (car n) 0))
        #t
        (begin (set-car! n (- (car n) 1))
               #f)))
  (let ((cell (list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-decrement cell)
                 (the-semaphore 'acquire)))
            ((eq? m 'release)
             (set-car! cell (+ (car cell) 1))) ;this is also atomic
            ((eq? m 'cell) cell)
            ))
    the-semaphore))

;--------------------------------------------------------------
;Section 3.4, Exercise 3.48

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'number) (account2 'number))
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))




;--------------------------------------------------------------
;Utility functions for this section:


(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'test-and-set)
             (test-and-set! cell))
            ((eq? m 'cell)
             cell)
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))



(define account-id
  (let ((currentID 0))
    (lambda ()
      (set! currentID (+ currentID 1))
      currentID)))


(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer
         (make-serializer))
        (account-number (account-id)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer)
             balance-serializer)
            ((eq? m 'number)
             account-number)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


