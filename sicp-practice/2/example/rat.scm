; 数据抽象能力(构造函数、选择函数)

; 有理数运算
; 有理数构造函数。a/b
(define (make-rat a b)
    (let ((g (gcd a b)))    ; 使用最大公约数来构造最简化的有理数
         (cons (/ a g) (/ b g))))
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

; 有理数选择函数
(define (numer rat)
    (car rat))
(define (denom rat)
    (cdr rat))

; 相加
(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

; 相减
(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

; 相乘
(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

; 相除
(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

; 是否相等
(define (equal-rat? x y)
    (= (* (numer x) (numer y))
       (* (denom x) (denom y))))


(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))


(define x (make-rat 1 2))
(define y (make-rat 1 4))

(add-rat x y)
(sub-rat x y)
(mul-rat x y)
(div-rat x y)
(equal-rat? x y)