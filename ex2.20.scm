(define (same-parity x . y)
  (let ((parity (even? x)))
    (define (iter y result)
      (if (null? y)
          result
          (if (eq? (even? (car y)) parity)
              (iter (cdr y) (append result (list (car y))))
              (iter (cdr y) result))))
    (iter y (list x))))

(same-parity 1 2 3 4 5 6 7)
