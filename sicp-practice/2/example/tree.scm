; 用序对构造树型结构

(define tree (cons (list 1 2) (list 3 4)))  ; ((1 2) 3 4)

; 序对大小
(length tree)   ; 3

; 计算树的节点个数
(define (count-leaves tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else (+ (count-leaves (car tree))
                   (count-leaves (cdr tree))))))
(count-leaves tree)


; Mapping(树结构映射)
(define (map f tree)
    (cond ((null? tree) ())
          ((not (pair? tree)) (f tree))
          (else (cons (map f (car tree)) (map f (cdr tree))))))
(map (lambda (x) (* x 10))
     (list 1 
           (list 2 (list 3 4) 5) 
           (list 6 7)))