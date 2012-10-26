(define (equal? a b)
  (if (or (and (null? a)
               (not (null? b)))
          (and (null? b)
               (not (null? a))))
      false
      (if (and (eq? (car a) (car b))
               (equal? (cdr a) (cdr b)))
          true
          false)))
