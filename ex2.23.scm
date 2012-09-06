#lang planet neil/sicp

(define (for-each proc li)
  (define (for-each-do proc li)
    (proc (car li))
    (for-each proc (cdr li)))
  (if (not (null? li))
      (for-each-do proc li)))


(for-each (lambda (x) (newline) (display x))
          (list 57 321 99))

