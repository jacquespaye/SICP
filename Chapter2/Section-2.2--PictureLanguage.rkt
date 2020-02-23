#lang sicp
(#%require sicp-pict)

;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.45


(define (split proc1 proc2)
  (define (splitProcedure painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitProcedure painter (- n 1))))
          (proc1 painter
                 (proc2 smaller smaller)))))
  splitProcedure)


;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.46

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1)
      (xcor-vect v2))
   (+ (ycor-vect v1)
      (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1)
      (xcor-vect v2))
   (- (ycor-vect v1)
      (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))



;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.47

;implementation 1

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;implementation 2

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))


;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.48


(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.49


(define outline
  (segments->painter
   (list
    (make-segment
     (make-vect 0 0)
     (make-vect 1 0))
    (make-segment
     (make-vect 1 0)
     (make-vect 1 1))
    (make-segment
     (make-vect 1 1)
     (make-vect 0 1))
    (make-segment
     (make-vect 0 1)
     (make-vect 0 0)))))


(define cross
  (segments->painter
   (list
    (make-segment
     (make-vect 0 1)
     (make-vect 1 0))
    (make-segment
     (make-vect 0 0)
     (make-vect 1 1)))))

(define diamond
  (segments->painter
   (list
    (make-segment
     (make-vect 0 0.5)
     (make-vect 0.5 1))
    (make-segment
     (make-vect 0.5 1)
     (make-vect 1 0.5))
    (make-segment
     (make-vect 1 0.5)
     (make-vect 0.5 0))
    (make-segment
     (make-vect 0.5 0)
     (make-vect 0 0.5)))))


(define wave
  (segments->painter
   (list
    (make-segment
     (make-vect 0.29 0)
     (make-vect 0.39 0.55))
    (make-segment
     (make-vect 0.39 0.55)
     (make-vect 0.2 0.49))
    (make-segment
     (make-vect 0.2 0.49)
     (make-vect 0 0.65))
    (make-segment
     (make-vect 0 0.74)
     (make-vect 0.2 0.6))
    (make-segment
     (make-vect 0.2 0.6)
     (make-vect 0.43 0.7))
    (make-segment
     (make-vect 0.43 0.7)
     (make-vect 0.35 0.85))
    (make-segment
     (make-vect 0.35 0.85)
     (make-vect 0.45 1))
    (make-segment
     (make-vect 0.55 1)
     (make-vect 0.65 0.85))
    (make-segment
     (make-vect 0.65 0.85)
     (make-vect 0.57 0.7))
    (make-segment
     (make-vect 0.57 0.7)
     (make-vect 1 0.39))
    (make-segment
     (make-vect 1 .29)
     (make-vect 0.6 .6))
    (make-segment
     (make-vect 0.6 .6)
     (make-vect 0.8 0))
    (make-segment
     (make-vect 0.65 0)
     (make-vect 0.5 0.3))
    (make-segment
     (make-vect 0.5 0.3)
     (make-vect 0.4 0))
    (make-segment
     (make-vect 0.45 0.8)
     (make-vect 0.55 0.8))
    
    )))


;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.51

(define (below painter1 painter2)
  (let ((splitPoint (make-vect 0 0.5)))
    (let ((paintBottom
           (transform-painter painter1
                              (make-vect 0 0)
                              (make-vect 1 0)
                              splitPoint))
          (paintTop
           (transform-painter painter2
                              splitPoint
                              (make-vect 1 0.5)
                              (make-vect 0 1))))
      (lambda (frame)
        (paintBottom frame)
        (paintTop frame)))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                                (rotate270 painter2))))




;--------------------------------------------------------------
;Section 2.1, Picture Exercises, Exercise 2.52
(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-vert
                         rotate180
                         identity
                         flip-horiz)))
    (combine4 (corner-split painter n))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1)))
            (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter up)
                  (below right corner)))))



;--------------------------------------------------------------
;Utility functions for this section:


(define einstein2 (beside einstein (flip-vert einstein)))
(define einstein4 (below einstein einstein))

(define (flipped-pairs painter)
  (let ((painter2
         (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter
                                  (- n 1))))
        (beside painter
                (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right
                         corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter)
                       quarter)))
    (below (flip-vert half) half))))

(define  (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let  ((top  (beside (tl painter)
                         (tr painter)))
           (bottom (beside (bl painter)
                           (br painter))))
      (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4
         (square-of-four identity
                         flip-vert
                         identity
                         flip-vert)))
    (combine4 painter)))

(define (square-limit2 painter n)
  (let ((combine4
         (square-of-four flip-horiz
                         identity
                         rotate180
                         flip-vert)))
    (combine4 (corner-split painter n))))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))






(define (flipVert painter)
  (transform-painter
   painter
   (make-vect 0 1)
   (make-vect 1 1)
   (make-vect 0 0)))

(define (shrinkUpperRight painter)
  (transform-painter
   painter
   (make-vect 0.5 0.5)
   (make-vect 1 0.5)
   (make-vect 0.5 1)))

(define (rotate90 painter)
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 1 1)
   (make-vect 0 0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.2 0.1)
                     (make-vect 0.1 0.2)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
  (let ((paint-left (transform-painter painter1
                                       (make-vect 0 0)
                                       split-point
                                       (make-vect 0 1)))
        (paint-right (transform-painter painter2
                                        split-point
                                        (make-vect 1 0)
                                        (make-vect 0.5 1))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame)))))




(define (square-limit2 painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))



(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))



(define  (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))







                        