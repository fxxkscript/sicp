#lang planet neil/sicp

(define square (lambda (x) (* x x)))
(define (square-tree tree)
  (tree-map square tree))

(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))