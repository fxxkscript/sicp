(define nil '())
(define (enumerate-interval begin end)
  (if (> begin end)
      nil
      (append (list begin) (enumerate-interval
                            (+ begin 1)
                            end))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (unique-pairs n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval
                           1
                           (- i 1))))
                   (enumerate-interval 1 n))))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))
;ec 2.41
(define (three-sum n)
  (filter (lambda (list)
            (= (accumulate + 0 list) n))
          (flat-map 
           (lambda (i)
             (flat-map (lambda (j)
                         (map (lambda (k)
                                (list i j k))
                              (enumerate-interval 1 (- j 1))))
                       (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n))))


;ex 2.42

(define empty-board nil)
(define (abs num)
  (if (> num 0)
      num
      (- num)))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define (safe? k positions)
  (define (two-queens-safe? a b)
    (and (not (= (car a) (car b)))
         (not (= (abs (- (car a) (car b)))
                 (abs (- (cadr a) (cadr b)))))))
  (let ((new-queen (last positions)))
    (define (check i positions)
      (cond ((= i k) true)
            ((two-queens-safe? (car positions) new-queen)
             (check (+ i 1) (cdr positions)))
            (else false)))
    (check 1 positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flat-map
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
(display "\n")
(display 
 ((lambda (list)
        (define (count li num)
          (if (null? li)
              num
              (count (cdr li) (+ num 1))))
        (count list 0))
      (queens 8)))
(display "\n")
