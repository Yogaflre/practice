; 素数(只能被1和自身整除)检测
; 用于检测一个整数n是否是素数

; 获取因子
(define (smallest-divisor n)
    (find-divisor n 2))
(define (find-divisor n guess)
    (cond ((> (square guess) n) n)  ; 理解
          ((= (remainder n guess) 0) guess)
          (else (find-divisor n (+ guess 1)))))

(smallest-divisor 6)
(smallest-divisor 11)
(smallest-divisor 15)
(smallest-divisor 41)

; 判断素数
(define (prime n)
    (= n (smallest-divisor n)))

(prime 6)
(prime 11)