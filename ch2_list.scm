(define one-four (list 1 2 3 4))

(define emptylist (list))

(define odd (list 1 3 5 7))

(define (list-ref list n) 
  (if (= 0 n) (car list) 
    (list-ref (cdr list) (- n 1))))

(define (list-len list) 
  (if (null? list) 0 (+ 1 (list-len (cdr list)))))


(define (append l1 l2) 
  (if (null? (cdr l1)) 
    (cons (car l1) l2)
    (cons (car l1) (append (cdr l1) l2))
  ))

(define (list-append ls) 
  (cond ((not (pair? ls)) ls)
        ((not (pair? (car ls))) ls)
        (else (append (car ls) (list-append (cdr ls)))
        )
  )
)

(define (last-pair li) 
  (if (null? (cdr li)) (car li) (last-pair (cdr li))))

(define (sub-list li k) 
  (if (= k 0) (list) 
    (cons (car li) 
          (sub-list (cdr li) 
                    (- k 1)
          ) 
    )
  )
)

(define (reverse-list li) 
  (if (null? li) (list)
    (cons (last-pair li) 
          (reverse-list (sub-list li (- (list-len li) 1))))
  )
)

(define (cc amount coin-values) 
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-coin coin-values))
                 (cc (- amount 
                        (first-coin coin-values)) 
                     coin-values)))
))

(define (no-more? coins) 
  (if (= (list-len coins) 0) #t #f)
)

(define (except-first-coin coins) 
  (cdr coins)
)

(define (first-coin coins) 
  (car coins))

;
; (define (main) 
;   (cc 100 (list 50 25 10 5 1)))

(define (try x . y) 
  (display x)
  (newline)
  (display y)
  (newline)
)

(define (same-parity first . l)
  (cons first (same-parity-iter first l))
 )
;
(define (same-parity-iter first l)
  (display first)
  (newline)
  (display l)
  (newline)

  (if (null? l) (list)
    (if (= (remainder first 2) (remainder (car l) 2))
      (cons (car l) (same-parity-iter first  (cdr l )))
      (same-parity-iter first (cdr l ))
    )
  )
)

; (define (square-list li) 
  ; (map (lambda (x) (* x x)) li))

(define (square-list items) 
  (if (null? items) (list) 
    (cons ((lambda (x) (* x x)) (car items)) 
          (square-list (cdr items))
    )
  )
)

; (define (main) (same-parity 1 2 3 4 5 6 7)) 
   ; (define (main) (try 1 2 3 4 5 6 7)) 
(define (main) (square-list (list 1 2 3 4)))
  

(define (deep-reverse ls) 
  (if (pair? ls) (reverse-list (map deep-reverse ls)) ls ))


(define (fringe ls) 
  ; (display ls)
  (if (flat-list? ls) ls (fringe (list-append ls)))
)

(define (first-element-not-list ls) 
  (if (pair? (car ls)) #f #t))

(define (flat-list? ls) 
  (if (or (not (pair? ls)) (null? (car ls)) ) #t
  (and (first-element-not-list ls) (flat-list? (cdr ls)))))

(define (make-branch len struct) 
  (list len struct))

(define (make-mobile left right) 
  (list left right))

(define (left-branch mobile) 
  (car mobile))

(define (right-branch mobile) 
  (car (cdr mobile)))

(define (branch-length mobile) 
  (car mobile))

(define (branch-struct mobile) 
  (car (cdr mobile)))

; (define (total-weight mobile)
;     (+ (branch-weight (left-branch mobile))         ; 计算左右两个分支的重量之和
;        (branch-weight (right-branch mobile))))
;
; (define (branch-weight branch)
;     (if (hangs-another-mobile? branch)              ; 如果分支吊着另一个活动体
;         (total-weight (branch-structure branch))    ; 那么这个活动体的总重量就是这个分支的重量
;         (branch-structure branch)))                 ; 否则， 分支的 structure 部分就是分支的重量
;
; (define (hangs-another-mobile? branch)              ; 检查分支是否吊着另一个活动体
;     (pair? (branch-structure branch)))

(define (total-weight mobile) 
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight mobile) 
  (if (pair? (branch-struct mobile)) 
    (total-weight (branch-struct branch)) 
    (branch-struct mobile)))

(define mobile (make-mobile (make-branch 10 20)       ; 活动体的总重量为 20 + 25 = 45
                            (make-branch 10 25)))


(define (main) 
  ; (append (list 1 2) (list 3 4))
  ; (flat-list? (list 1 2 3 4 5))
  ; (flat-list? (list (list 1 2) (list 3 4) (list 5 6)))
  ; (list-append (list (list (list 1 2) (list 3 4)) (list (list 3 4) 5) ))
  ; (fringe (list (list (list 1 2) (list 3 4)) (list (list 3 4) 5) ))
  (total-weight mobile)
)


; (define (main) 
;   emptylist
;   ; (list-len one-four)
;   (sub-list one-four 2)
;   (reverse-list one-four)
; )

