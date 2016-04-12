(define (make-interval a b) (cons a b))

(define (lower-bound inv) (min (car inv) (cdr inv)))

(define (upper-bound inv) (max (car inv) (cdr inv)))

(define (add-interval x y)
  ; (make-interval (+ (lower-bound x) (lower-bound y))
  ;                (+ (upper-bound x) (upper-bound y))))
  (interval-opr x y +))
(define (sub-interval x y) 
  (interval-opr x y (lambda (x y) (abs (- x y)))))
(define (mul-interval x y)
  (interval-opr x y *))
(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (interval-opr x y opr) 
  (let ((p1 (opr (lower-bound x) (lower-bound y)))
        (p2 (opr (lower-bound x) (upper-bound y)))
        (p3 (opr (upper-bound x) (lower-bound y)))
        (p4 (opr (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (main)
  (define x (make-interval 1.0 3.0))
  (define y (make-interval 1.5 2.5))
  (display (lower-bound (sub-interval x y)))
  (display (upper-bound (sub-interval x y)))
)
