(define (split a1 a2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split a1 a2) painter (- n 1))))
          (a1 painter (a2 smaller smaller))))))
