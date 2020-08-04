; 过程抽象

; 计算a-b的和
(define (sum a b)
    (if (> a b)
        0
        (+ a (sum (+ a 1) b))))
(sum 1 3)   ; 6

; 计算a-b的平方和
(define (square-sum a b)
    (if (> a b)
        0
        (+ (square a) (square-sum (+ a 1) b))))
(square-sum 1 3)    ; 14


; 上面两个过程是高度类似的，所以我们可以抽象成一个公共的procedure
(define (common-sum a b func next)
    (if (> a b)
        0
        (+ (func a) (common-sum (next a) b func next))))
(common-sum 1 3 (lambda (x) x) (lambda (x) (+ x 1)))    ; 6
(common-sum 1 3 (lambda (x) (square x)) (lambda (x) (+ x 1)))   ; 14


; 使用let构造局部变量
; 普通格式：
; (let ((⟨var₁⟩ ⟨exp₁⟩)
;       (⟨var₂⟩ ⟨exp₂⟩)
;       …
;       (⟨varₙ⟩ ⟨expₙ⟩))
;   ⟨body⟩)
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f 1 2)