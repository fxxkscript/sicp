#lang planet neil/sicp

(define nil '()) 
(define x (list (list 1 2) 3 4))

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items)
                    result))))
  (iter items nil))
 
(define (deep-reverse z)
  (cond ((null? z) nil)
        ((pair? (car z))
         (append (deep-reverse (cdr z))
                 (list (deep-reverse (car z)))))
        (else
         (append (deep-reverse (cdr z))
                 (list (car z))))))

(deep-reverse x)
         
            
 
