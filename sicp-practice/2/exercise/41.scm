; Write a procedure to find all ordered triples of distinct positive integers i , j , and k less than or equal to a given integer n that sum to a given integer s.

(define (accumulate op init items)
    (if (null? items)
        init
        (op (car items)
            (accumulate op
                        init
                        (cdr items)))))

(define (enumerate-interval low high)
    (if (> low high)
        ()
        (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
    (accumulate append () (map proc seq)))

; 生成所有相异的三元组
(define (make-triples n)
    (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                                   (enumerate-interval 1 (- j 1))))
                                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

; 求三元组的和
(define (triple-sum triple)
    (+ (car triple) (cadr triple) (caddr triple)))

; 判断是否为整数s
(define (is-s? triple s)
    (= (triple-sum triple) s))

; 寻找和等于s的不重复三元组
(define (unique-triples n s)
    (filter (lambda (triple) (is-s? triple s))
            (make-triples n)))

(unique-triples 13 10)  ; ((5 3 2) (5 4 1) (6 3 1) (7 2 1))