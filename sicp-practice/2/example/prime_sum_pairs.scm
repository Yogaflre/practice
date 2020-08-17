; 给定自然数n，找出所有不同的序对i和j，使得i+j是素数
; 聚合函数
(define (accumulate op init items)
    (if (null? items)
        init
        (op (car items)
            (accumulate op
                        init
                        (cdr items)))))

; 递增序列
(define (enumerate-interval low high)
    (if (> low high)
        ()
        (cons low (enumerate-interval (+ low 1) high))))

; 1.产生所有序对(利用了双层递归)
(define (make-all-pairs n)
    (accumulate append () (map (lambda (i)
                                       (map (lambda (j) (list i j))
                                                        (enumerate-interval 1 (- i 1))))
                                       (enumerate-interval 1 6))))

; 2.计算序对的和
(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (smallest-divisor n)
    (find-divisor n 2))
(define (find-divisor n guess)
    (cond ((> (square guess) n) n)  ; 理解
          ((= (remainder n guess) 0) guess)
          (else (find-divisor n (+ guess 1)))))
; 3.过滤素数
(define (prime? list)
    (= (cadr (cdr list)) (smallest-divisor (cadr (cdr list)))))


; 求所有序对和
(define (prime-sum-pairs n)
    (filter prime?  ; 过滤素数
            (map make-pair-sum  ; 构造所有序对和
                 (make-all-pairs n))))  ; 生成所有序对

(prime-sum-pairs 6)