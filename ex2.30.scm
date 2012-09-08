#lang planet neil/sicp
(define square (lambda (x) (* x x)))
(define (square-tree x)
  (cond ((null? x) nil) 
        ((not (pair? x)) (square x))
        (else
         (cons 
          (square-tree (car x))
          (square-tree (cdr x))))))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree))) tree))

(square-tree-2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))