(define (cons-c x y) 
  (lambda (m) (m x y)))

(define (car-c m) 
  (m (lambda (x y) x)))



(define (accumulate op initial sequence) 
  (if (null? sequence) initial 
    (op (car sequence) 
        (accumulate op initial (cdr sequence)))))

; (define (main) (accumulate + 0 (list 1 2 3 4 5)))

(define (append l1 l2)
  (cond ((null? l1) l2)
        ((not (pair? l1)) (cons l1 l2))
        (else (cons (car l1) (append (cdr l1) l2)))
  )
)

(define (list-append ls) 
  (cond ((not (pair? ls)) ls)
        ((not (pair? (car ls))) ls)
        (else (append (car ls) (list-append (cdr ls))))
  )
)

; (define (main) (append (list 1 2 3 4) (list 6 7 8)))

(define (enumerate-tree tree) 
  (cond ((null? tree) (list)) 
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))
              )
        )
  )
)

(define (filter-list predict array) 
  (cond ((null? array) (list))
        ((predict (car array)) (cons (car array) (filter-list predict (cdr array))))
        (else (filter-list predict (cdr array)))))

(define (horner-eval x fx) 
  (accumulate (lambda (c cfx) (+ c (* x cfx) )) 0 fx))

(define (last-element li) 
  (if (null? (cdr li)) 
    (car li) 
    (last-element (cdr li))))

(define (first-element-not-list ls) 
  (if (pair? (car ls)) #f #t))

(define (flat-list? ls) 
  (if (or (not (pair? ls)) (null? (car ls)) ) #t
  (and (first-element-not-list ls) (flat-list? (cdr ls)))))

(define (flat-tree tree) 
  (display tree)
  (newline)
  (if (flat-list? tree) tree (flat-tree (flat-tree-iter tree)))
)

(define (flat-tree-iter tree) 
  (display tree)
  (newline)
  (cond ((null? tree) tree)
    (else (append (car tree) (flat-tree-iter (cdr tree))))
  ))


(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (sub-tree)
                         (if (pair? sub-tree)           ; 如果这个节点有分支
                             (count-leaves sub-tree)    ; 那么这个节点调用 count-leaves 的结果就是这个节点的树叶数量
                             1))                        ; 遇上一个叶子节点就返回 1
                     tree)))

; (define (main) (horner-eval 2 (list 1 2 3)))

; (define (main) (reverse (list 1 2 3)))

; (define (main) (append (list 1 (list 2 (list 3 4 (list 5 6)))) (list 1 2 3 4)))

; (define (main) (enumerate-tree (list 1 (list 2 (list 3 4 (list 5 6))))))

; (define (main) 
;   (filter-list (lambda x (= (remainder x 2) 1)) (list 1 2 3 4 5)))



(define (car-n seqs) 
  (accumulate (lambda (x y) (cons (car x) y)) (list) seqs))

(define (cdr-n seqs) (accumulate (lambda (x y) (cons (cdr x) y)) (list) seqs))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        (list)
        (cons (accumulate op init (car-n seqs))
              (accumulate-n op init (cdr-n seqs)))))

; (define (main) (car-n test-lists))

; (define (main) (cdr-n test-lists))

; (define test-lists (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define m (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(define m2 (list (list 1 2 3 4) (list 5 6 7 8)))

(define v (list 1 2 3 4))

(define w (list 5 6 7 8))

; (define (main) (count-leaves (list 1 (list 8 9) (list 2 (list 3 4 (list 5 6))))))

; (define (main) (accumulate-n + 0 test-lists))

; (define (main) (map * w v))

(define (dot-product w v) 
  (accumulate + 0 (map * w v)))

; (define (matrix-*-vector m v) 
;   (if (null? m) 
;     (list) 
;     (cons (dot-product (car m) v) 
;           (matrix-*-vector (cdr m) v))))

(define (matrix-*-vector m v) 
  (map (lambda (v1) (dot-product v1 v)) m))

(define (transpose m) 
  (accumulate-n cons (list) m)
  )

(define (matrix-*-matrix m1 m2) 
  (let ((cols (transpose m2)))
    (display cols)
    (newline)
    (map (lambda (l) (matrix-*-vector m1 l)) cols))
)
(define (main) (matrix-*-matrix m m2))

; (define (main) (transpose m))

