#lang planet neil/sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (iter z x result)
  (if (= (remainder z x) 0)
      (iter (/ z x) x (+ result 1))
      result))

(define (car z)
  (iter z 2 0))

(define (cdr z)
  (iter z 3 0))

(display (car (cons 234 123)))
(newline)
(display (cdr (cons 234 123)))
