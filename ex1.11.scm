(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))  (* (f (- n 2)) 2) (* (f (- n 3)) 3))))

(define (f1 n)
  (f1-iter 0 1 2 3 n))

(define (f1-iter first second third count max-count)
  (if (< max-count 3)
      max-count
      (if (> count max-count)
          third
          (f1-iter second third (+ (* first 3) (* second 2) third) (+ count 1) max-count)
          )))
