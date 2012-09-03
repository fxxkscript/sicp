(define (last-pair l)
  (let ((a (cdr l)))
  (if (null? a)
      (car l)
      (last-pair a))))

(display (last-pair (list 123 42 54 13)))
