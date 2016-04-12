(define a-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree a-tree) 
  (cond
    ((null? a-tree) (list))
    ((not (pair? a-tree)) (* a-tree a-tree) )
        (else (cons (square-tree (car a-tree))
                (square-tree (cdr a-tree)))
     ) 
))

(define (tree-map f a-tree) 
  (cond
    ((null? a-tree) (list))
    ((not (pair? a-tree)) (f a-tree) )
        (else (cons (tree-map f (car a-tree))
                (tree-map  f (cdr a-tree)))
     ) 
))


; (define (square-tree a-tree) 
;   (if (not (pair? a-tree)) 
;     (* a-tree a-tree)
;     (map square-tree a-tree)
; ))
;
;

; (define (tree-map f a-tree) 
;   (define ())
;   (if (not (pair? a-tree)) 
;     ()))



(define (main) 
  ; (square-tree a-tree)
  (tree-map (lambda (x) (* x x)) a-tree)
)
