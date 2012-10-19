#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (car z))
(define (lower-bound z)
  (cdr z))
