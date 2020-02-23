#lang sicp

;--------------------------------------------------------------
;Section 3.3, Digital Circuit Simulator, Exercise 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                       (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)



;--------------------------------------------------------------
;Section 3.3, Digital Circuit Simulator, Exercise 3.30

;;binary addition implementation;;

(define (ripple-carry aList bList sList c)
  (define (adder-iter aList bList sList c-in)
    (if (null? (cdr aList))
        (full-adder (car aList)
                    (car bList)
                    c-in
                    (car sList)
                    c)
        (let ((c-out (make-wire)))
          (full-adder (car aList)
                      (car bList)
                      c-in
                      (car sList)
                      c-out)
          (adder-iter (cdr aList)
                      (cdr bList)
                      (cdr sList)
                      c-out))))
  (let ((c-initial (make-wire)))
    (adder-iter aList bList sList c-initial)))


;--------------------------------------------------------------
;Section 3.3, Digital Circuit Simulator, Exercise 3.33


(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (multiplier u c v)
    (adder a b v)
    (constant 2 u))
  'ok)



;--------------------------------------------------------------
;Section 3.3, Digital Circuit Simulator, Exercise 3.34,3.35
(define (square z) (* z z))
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER"
                   (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (square (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;--------------------------------------------------------------
;Section 3.3, Digital Circuit Simulator, Exercise 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))


(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (cv value)
  (let ((z (make-connector)))
    (constant value z)
    z))

  
(define (cfCon x)
    (c+ (c* (c/ (cv 9) (cv 5))
            x)
        (cv 32)))


;--------------------------------------------------------------
;Utility functions for this section:

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal)
             signal-value)
            ((eq? m 'set-signal!)
             set-my-signal!)
            ((eq? m 'add-action!)
             accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))




(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))



;;queue implementation;;
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
      (error "FRONT called with an empty queue" queue)
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

;;end queue implementation;;


(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue!
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment
                      time
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment
                time
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments!
         agenda
         (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg
             (first-segment agenda)))
        (set-current-time!
         agenda
         (segment-time first-seg))
        (front-queue
         (segment-queue first-seg)))))

(define the-agenda (make-agenda))

;;gate definitions;;;
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


       

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (logical-and s1 s2)
  (cond ((and
          (= s1 1)
          (= s2 1)) 1)
        ((or
          (= s1 0)
          (= s2 0)) 0)
        (else (error "Invalid signals:" s1 s2))))
(define (logical-or s1 s2)
  (cond ((or
          (= s1 1)
          (= s2 1)) 1)
        ((and
          (= s1 0)
          (= s2 0)) 0)
        (else (error "Invalid signals:" s1 s2))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value
           (logical-not (get-signal input))))
      (after-delay
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (probeWire name wire)
  (add-action!
   wire
   (lambda ()
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display " New-value = ")
     (display (get-signal wire))
     (newline))))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (makeWireList digitList)
  (define (makeWireListIter digitList resultList)
    (cond ((null? digitList) resultList)
          (else (let ((wire (make-wire)))
                  (set-signal! wire (car digitList))
                  (makeWireListIter (cdr digitList) (cons wire resultList))))))
  (makeWireListIter digitList '()))

(define (getSignals wireList)
  (define (getSignalsIter wireList results)
    (cond ((null? wireList) results)
          (else (getSignalsIter (cdr wireList)
                                (cons
                                 (get-signal (car wireList))
                                 results)))))
  (getSignalsIter wireList '()))

(define (convertToBinary number)
  (define (convertIter number result)
    (cond ((= number 0) result)
          (else (convertIter (floor (/ number 2)) (cons
                                                   (remainder number 2)
                                                   result)))))
  (convertIter number '()))

(define (convertToInteger binaryList)
  (define (convertIter binaryList result factor)
    (cond ((null? binaryList) result)
          (else (convertIter (cdr binaryList)
                             (+ result
                                (* factor (car binaryList)))
                             (* 2 factor)))))
  (convertIter (reverse binaryList) 0 1))

(define (makeZeros list)
  (define (zerosIter list result)
    (if (null? list) result
        (zerosIter (cdr list) (cons 0 result))))
  (zerosIter list '()))

(define (padWithZeros shorter longer)
  (cond ((= (length shorter)
            (length longer)) shorter)
        (else  (padWithZeros (cons 0 shorter)
                             longer))))

(define (makeEqualBinaries a b)
  (let ((aBinary (convertToBinary a))
        (bBinary (convertToBinary b)))
    (cond ((> (length aBinary)
              (length bBinary))
           (cons aBinary
                 (padWithZeros bBinary aBinary)))
          ((< (length aBinary)
              (length bBinary))
           (cons (padWithZeros aBinary bBinary)
                 bBinary))
          ((= (length aBinary)
              (length bBinary))
           (cons aBinary bBinary))
          (else (error "Couldn't compare binary lengths")))))

;;creates a ripple carry adder and uses it to add the two integers provided
(define (add a b)
  (let* ((binaries (makeEqualBinaries a b))
         (aBinary (car binaries))
         (bBinary (cdr binaries))
         (sumBinary (makeZeros aBinary)))
    (if (= (length aBinary)
           (length bBinary))
        (let ((c (make-wire))
              (aWires (makeWireList aBinary))
              (bWires (makeWireList bBinary))
              (sumWires (makeWireList sumBinary)))
          (probeWire "last digit" c)
          (ripple-carry aWires bWires sumWires c)
          (propagate)
          (convertToInteger
           (cons
            (get-signal c)
            (getSignals sumWires))))
        (error "Not same lengths"))))


;;;end ripple adder


;;constraint propagator;;



(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1)
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1)
                          (get-value a2))
                       me))
          ((and (has-value? a1)
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum)
                          (get-value a1))
                       me))
          ((and (has-value? a2)
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum)
                          (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum  me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1)
                    (= (get-value m1) 0))
               (and (has-value? m2)
                    (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1)
                (has-value? m2))
           (set-value! product
                       (* (get-value m1)
                          (get-value m2))
                       me))
          ((and (has-value? product)
                (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product)
                (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define  (print-probe value)
    (display "Probe: ")
    (display name) (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except
              setter
              inform-about-value
              constraints))
            ((not (= value newval))
             (error "Contradiction"
                    (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except
                  retractor
                  inform-about-no-value
                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint
                     constraints))
          (set! constraints
                (cons new-constraint
                      constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!)
             set-my-value)
            ((eq? request 'forget)
             forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))




(define (for-each-except exception
                         procedure
                         list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector
                    new-value
                    informant)
  ((connector 'set-value!)
   new-value
   informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))



(define (celsius-farenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define Cels (make-connector))
(define Far (make-connector))
(celsius-farenheit-converter Cels Far)


(define A (make-connector))
(define B (make-connector))
(define C (make-connector))





  
;;function that takes an input connector and will apply any
;;function to the value before outputting the result

(define (apply-function input output f)
  (define (process-new-value)
    (cond ((has-value? input)
           (set-value! output (f (get-value input)) me))))
  (define (process-forget-value)
    (forget-value! input me)
    (forget-value! output me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: APPLY-FUNCTION:" request))))
  (connect input me)
  (connect output me)
  me)
  


  





