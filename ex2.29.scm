#lang planet neil/sicp
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)
(define (mobile? x)
  (pair? (car x)))
(define (weight? x)
  (and (not (mobile? x))
       (not (pair? (branch-structure x)))))
(define (total-weight x)
  (cond ((weight? x) (branch-structure x))
        ((mobile? x)
         (+ (total-weight (left-branch x))
            (total-weight (right-branch x))))
        (else
         (total-weight (branch-structure x)))))

; test total-weight      
(define m1 (make-mobile (make-branch 1 48)
                        (make-branch 4 12)))
(total-weight m1)
;Value: 60

(define m2 (make-mobile (make-branch 6 m1) 
                        (make-branch 9 40)))
(total-weight m2)
;Value: 100

(define m3 (make-mobile (make-branch 3 140) 
                        (make-branch 7 60)))
(total-weight m3)
;Value: 200

(define m4 (make-mobile (make-branch 20 m2)
                        (make-branch 10 m3)))
(total-weight m4)
;Value:: 300