(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((exponentation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentation (base exp)
                                (- (exponent exp) 1))
           (deriv (base exp) var))))
        ((product? exp)
         (make-sum (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp)
                           var))
                   (make-product
                    (multiplicand exp)
                    (deriv (multiplier exp)
                           var))))))
(define (variable? exp)
  (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0))
         0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else
         (list '* a1 a2))))

(define (=number? a1 a2)
  (and (number? a1) (number? a2) (= a1 a2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '* (cddr s))))

(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
(define (make-exponentation u n)
  (list '** u n))
(define (exponentation? x)
  (and (pair? x) (eq? (car x) '**))) 
                                        ;test
(newline)
(display (deriv '(+ x 3) 'x))
(newline)
(display (deriv '(* x y) 'x))
(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
(display (deriv '(* x y (+ x 3)) 'x))
(newline)
(display (deriv '(** x 10) 'x))
(newline)
