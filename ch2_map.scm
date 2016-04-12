(define test-list (list 1 2 3 4 5 6 7 8 9 10))

(define (cons-c x y) 
  (lambda (m) (m x y)))

(define (car-c m) 
  (m (lambda (x y) x)))

; (define (main) 
;   (define c (cons-c 1 2))
;   (car-c c)
; )

(define (append l1 l2)
  (cond ((null? l1) l2)
        ((not (pair? l1)) (cons l1 l2))
        (else (cons (car l1) (append (cdr l1) l2)))
  )
)

(define (accumulate op initial sequence) 
  (if (null? sequence) initial 
    (op (car sequence) 
        (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq) 
  (display "flatmap")
  (newline)
  (display seq)
  (display (proc seq))
  (display (map proc seq))
  (newline)
  (accumulate append (list) (map proc seq)))


(define (smallest-divisor x) 
  (find-divisor x 2))

(define (find-divisor n test) 
  (cond ((> (square test) n) n)
        ((divides? test n) test)
        (else (find-divisor n (+ test 1)))
        ))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (divides? test n) 
  (= (remainder n test) 0))

(define (enum-int interv init n)
  (if (> init n) 
    (list) 
    (cons init (enum-int interv (+ init interv) n)) )
  )

(define (prime-sum? pair) 
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair) 
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n) 
  (map make-pair-sum 
       (filter prime-sum? 
               (flatmap (lambda (i) 
                          (map (lambda (j) (list i j)) 
                               (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (enumerate-interval k n)
  (display "size:")
  (display n)
  (newline)
  (enum-int k 1 n))



(define (flat-map-number n) 
  (accumulate append 
             ; (list)  
             (map (lambda (i) 
                    (map (lambda (j) (list i j)) 
                         (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n)
                  )
             )
)

(define (unique-pairs n) (flat-map-number n))


(define (car-p p) ())

; (define (three-sum n) 
;   (accumulate append 
;               (list) 
;               (map (lambda (li) (list (car li) 
;                                            (cadr li) 
;                                            (- n (car li) (cadr li))))  
;                    (unique-pairs (- n 1)))))

(define (three-combo n) 
  (map (lambda (li) (list (car li) (cadr li) (- n (car li) (cadr li)))) (unique-pairs (- n 1))))

(define (three-sum n) (filter (lambda (li) (> (caddr li) 0)) (three-combo n)))

; (define (main) (prime-sum-pairs 30))

(define (safe? k position)
    (iter-check (car position) 
                (cdr position)
                 1))

(define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)  ; 下方所有皇后检查完毕，新皇后安全
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
            (if (or (= row-of-new-queen row-of-current-queen)           ; 行碰撞
                    (= row-of-new-queen (+ i row-of-current-queen))     ; 右下方碰撞
                    (= row-of-new-queen (- row-of-current-queen i)))    ; 左下方碰撞
                #f
                (iter-check row-of-new-queen 
                            (cdr rest-of-queens)    ; 继续检查剩余的皇后
                            (+ i 1))))))            ; 更新步进值




(define (queens broad-size) 
 (define (queen-cols k) 
  (display k)
  (newline)
  (if (= k 0) 
     (list (list)) 
     (filter 
       (lambda (positions) (safe? k positions)) 
          (flatmap 
            (lambda (rest-of-queens) 
              (map (lambda (new-row) 
                     (cons new-row rest-of-queens)) 
                   (enumerate-interval 1 broad-size))
            )
           (queen-cols (- k 1)))
      )
     )
  )
  (queen-cols broad-size))

; (define (main) (three-sum 20))

(define (main) (queens 8))
