(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))
(display (square-list (list 1 2 3)))
(display (square-list-2 (list 1 2 3)))
