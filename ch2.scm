(load "ch1.scm")

(define (common-divider a b)
  (if (= b 0) a (common-divider b (remainder a b))))

(define (CD a b) (if (> a b) 
                   (common-divider a b) 
                   (common-divider b a)))

(define (make-rat-sign x y sign)
  (let ((n (CD x y))) 
    (cons (sign (/ x n)) (/ y n))
  )
)

(define (make-rat x y) 
  (if (> (* x y) 0) (make-rat-sign (abs x) (abs y) +) 
    (make-rat-sign (abs x) (abs y) -))
  )

(define (numer rat) (car rat))

(define (demon rat) (cdr rat))

(define (print-rat rat) 
  (newline)
  (display (numer rat))
  (display "/")
  (display (demon rat)))

(define (add-rat a b) 
  (oper-rat a b +))

(define (sub-rat a b) 
  (oper-rat a b -))

(define (mul-rat a b) 
  (make-rat (* (numer a) (numer b))
            (* (demon a) (demon b))
  )
)

(define (div-rat a b) 
  (make-rat (* (numer a) (demon b)) 
            (* (numer b) (demon a))))

(define (oper-rat a b opr)
  (make-rat (opr (* (numer a) (demon b)) 
                 (* (numer b) (demon a))) 
            (* (demon a) (demon b))))

(define (equ-rat? a b) 
  (= (* (numer a) (demon b)) 
     (* (numer b) (demon a))
  )
)

(define (make-point x y) (cons x y))
(define (make-segment start end) 
  (cons start end))
(define (make-segment-p x1 y1 x2 y2) 
  (let ((p1 (make-point x1 y1))
        (p2 (make-point x2 y2)))
    (make-segment p1 p2)
  )
)


(define (start-segment segment) 
  (car segment))
(define (end-segment segment) 
  (cdr segment))

(define (x-point p) 
  (car p) )

(define (y-point p) 
  (cdr p))

(define (mid-point segment) 
  (make-point (average (x-point (start-segment segment)) 
                       (x-point (end-segment segment))) 
              (average (y-point (start-segment segment)) 
                       (y-point (end-segment segment)))
  )
)

(define (print-point p) 
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)

(define zero (lambda (f) (lambda (x) x)))

(define add-one 
  (lambda (n) 
    (lambda (f) 
      (lambda (x) (f ((n f) x)) ))))

(define )

