; 递归求导数
(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp) (make-sum (make-product (multiplier exp)
                                                  (deriv (multiplicand exp) var))
                                    (make-product (deriv (multiplier exp) var)
                                                  (multiplicand exp))))
          ((exponentiation? exp) (let ((u (base exp))
                                       (n (exponent exp)))
                                      (make-product (make-product n
                                                                  (make-exponentiation u (- n 1)))
                                                    (deriv u var))))
          (else (error "unknown expression type -- DERIV" exp))))

; 补充求导过程中的procedure
; 判断是否为变量
(define (variable? v)
    (symbol? v))

; 判断变量是否相等
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

; 构造和式
(define (make-sum a1 a2)
    (cond ((and (number? a1) (= a1 0)) a2)
          ((and (number? a2) (= a2 0)) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
; 构造乘式
(define (make-product a1 a2)
    (cond ((or (and (number? a1) (= a1 0)) (and (number? a2) (= a2 0))) 0)
          ((and (number? a1) (= a1 1)) a2)
          ((and (number? a2) (= a2 1)) a1)
          ((and (number? a1) (number? a2)) (* a1 a2))
          (else (list '* a1 a2))))

; 判断和式
(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
; 被加数和加数
(define (addend x) (cadr x))
(define (augend x) (caddr x))

; 判断乘式
(define (product? x)
    (and (pair? x) (eq? (car x) '*)))
; 被乘数和乘数
(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))

; 测试
(deriv '(+ (* 2 x) 1) 'x)
(deriv '(* x y) 'x)


; exercise-2.56 添加新的求导公式
(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))
(define (make-exponentiation base exponent)
    (cond ((= exponent 0) 1)
          ((= exponent 1) base)
          (else (list '** base exponent))))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(deriv '(** x 0) 'x)
(deriv '(** x 2) 'x)
(deriv '(** x 3) 'x)