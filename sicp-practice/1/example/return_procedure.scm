; 将procedure作为返回值

; 接收f函数，返回匿名函数
(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (average x y)
    (/ (+ x y) 2))

((average-damp square) 10)  ; 55