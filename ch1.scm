(define (square x) (* x x))

(define (abs x) (
    if(> x 0) x (- x) ))

(define (sqrt-iter guess x)
  (if (good-enough-fn? guess x square)
    guess 
    (sqrt-iter (improve guess x) x
  )))

(define (improve guess x) (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (good-enough-fn? guess x fn ) 
  (< (abs (- (fn guess) x)) 0.001))

(define (good-enough? guess x) 
  (< (abs (- (square guess) x)) 0.001 ))

(define (sqrt x) (sqrt-iter 1.0 x))

(define (cube-root-iter guess x) 
  (if (good-enough-fn? guess x cube)
    guess (cube-root-iter (cube-improve guess x) x)))

(define (cube x) (* x x x))

(define (cube-improve y x) (/ (+ (/ x (square y)) (* 2 y)) 3) )

(define (cube-root x) (cube-root-iter 1.0 x))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) 
                 (A x (- y 1))))))

(define (ftree1 n) 
  (cond ((< n 3) n)
        (else (+ (ftree1 (- n 1)) 
                 (* 2 (ftree1 (- n 2))) 
                 (* 3 (ftree1 (- n 3))) ))
        ))

(define (ftree2 n) (ftree-iter 2 1 0 0 n))

(define (ftree-iter a b c i n) 
  (if (= n i) c
    (ftree-iter (+ a (* 2 b) (* 3 c)) 
                a
                b 
                (+ i 1)
                n)))

(define (even? x) (= (remainder x 2) 0))

(define (exp-fast b n)
  (if (= n 0) 1 (exp-fast-iter b b n)))

(define (exp-fast-iter product b n)
  (cond ((= n 1) product)
        ((even? n) (exp-fast-iter (square product) b (/ n 2)))
        (else (* b (exp-fast-iter  product  b (- n 1))))
  ))

(define (calc oper x count) 
  (calc-iter oper x 0 count))

(define (calc-iter oper x product count) 
  (cond ((= 0 count) product) 
        ((even? count) ())))

(define (smallest-divisor x) 
  (find-divisor x 2))

(define (find-divisor n test) 
  (cond ((>= (square test) n) n)
        ((divides? test n) test)
        (else (find-divisor n (+ test 1)))
        ))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (divides? test n) 
  (= (remainder n test) 0))

(define (timed-prime-test n) 
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n) 
    (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (newline)
  (display "***")
  (newline)
  (display elapsed-time))

(define (search-for-primes-test x)
  (search-for-primes x (runtime)))

(define (search-for-primes start start-time) 
  (if (prime? start) (report-prime start (- (runtime) start-time)) 
    (search-for-primes (+ start 1) start-time) ))

; (define (func-sum func start next end)
;   (if (> start end ) 0 
;     (+ (func start) (func-sum func (next start) next end)
;        )))

(define (next-one x) (+ x 1))

(define (cubsum a b)
  (func-sum cube a next-one b))

(define (calc f a b dx) 
  (define (add-dx x) (+ x dx))
  (* (func-sum f (+ a (/ dx 2.0)) add-dx b) dx)) 

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (fn k) 
    (if (= 0 k) 
      (y k)
      (if (even? k) (* 2 (y k)) 
                    (* 4 (y k)))
    )
)
  (* (/ h 3.0) (func-sum fn 0 next-one n)))

(define (func-sum-iter term a next b) 
  (define (iter x result) 
    ( if (= x b) result 
                 (iter (next x) (+ result (term x))) ))
  (iter a 0))

; (define (product f a next b) 
;   (if (= a b) (f b) 
;               (* (f a) (product f (next a) next b))))

(define (product-iter-iter f a next b product) 
  (if (= a b) product 
    (product-iter-iter f (next a) next b (* (f a) product)) ))

(define (product-iter f a next b) 
  (product-iter-iter f a next b 1))

(define (calc-pi n) 
  (define (fact-func n) 
    (if (even? n) (/ (+ 2.0 n) (+ n 1.0))
                  (/ (+ n 1.0) (+ n 2.0))))
  (product-iter fact-func 1 next-one n))

; (define (accumulate combiner init-value term a next b) 
;   (if (= a b) init-value 
;               (accumulate combiner (combiner init-value (term a)) term (next a) next b)))

(define (accumulate combiner term a next b) 
  (if (= a b) (term b)
              (combiner (term a) (accumulate combiner term (next a) next b))))

(define (filter-accumulate filter combiner term a next b) 
  (define (filter-combiner a b) (if (filter a) (combiner a b) 
                                               b ))
  (accumulate filter-combiner term a next b)
)


(define (func-sum func start next end)
  ; (accumulate + 0 func start next end ))
  (accumulate + func start next end ))

(define (product func start next end)
  ; (accumulate * 1 func start next end ))
  (accumulate *  func start next end ))

(define (self x) x)

(define (prime-sum a b) 
  (filter-accumulate prime? + self a next-one b))

(define tolerance 0.0001)

(define (fixed-point f first-guess) 
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count) 
    (display guess)
    (display "  ")
    (display count)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next (try next (+ 1 count)))))
  (try first-guess 1))

(define (sqrt-fix x) 
  (fixed-point 
    (lambda (y) (average y (/ x y))) 1.0))

(define (golden) 
  (fixed-point (lambda (y) (+ 1.0 (/ 1.0 y))) 1.0))

(define (log-point number) 
  (fixed-point (lambda (x) (average (/ (log number) (log x)) x) ) 2.0))

(define (cont-fact n d k) 
  (count-fact-iter n d k 1 ))

(define (count-fact-iter n d k count)
  (if (= count k) (/ (n k) (d k)) 
    (/ (n count) (+ (count-fact-iter n d  k (+ count 1)) (d count))) ))

(define (count-fact-iter-n n d k count product) 
  (if (= count k) product 
    (count-fact-iter-n n d k (+ count 1) (/ n (+ 1 product)))))

(define (test) (cont-fact (lambda (x) 1.0) (lambda (x) 1.0) 1000))

(define (e-cal) 
  (define (D x)
    (if (= (remainder (+ x 1) 3) 0) (* 2.0 (/ (+ x 1) 3)) 1.0))
  (cont-fact (lambda (x) 1.0) D 1000))

(define (tan-cf x k)
   (define (N y) (if (= y 1) x (square x)))
   (define (D y) (- (* 2 y) 1))
   (cont-fact N D k))

(define (derv g) 
  (define dx 0.00001) 
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g) 
  (lambda (x) 
    (- x (/ (g x) ((derv g) x)))))

(define (main) ((repeated square 3) 2))
; (define (newton-method f ))

(define (newton-method-new g guess) 
  (fixed-point (newton-transform g) guess))

(define (cubic a b c) (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n) 
  (if (= n 1) f 
    (lambda (x) (f ((repeated f (- n 1)) x)))))




