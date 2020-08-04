; 用牛顿法求解平方根
(define (sart x)
    (newton (lambda (y) (- x (square y))) 1))

; 求不动点
(define (newton f guess)
    (define df (deriv f))
    (fixed-point (lambda (x) (- x (/ (f x) (df x)))) guess))    ; WHY 这里并没有定义fixed-point，为什么可以运行？

; 求导
(define (deriv f)
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define dx 0.00001)


(sqrt 9)