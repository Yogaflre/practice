#lang racket

; 定义递增、递减和判断是否为0的函数
(define (add1 x)
  (+ x 1))
(define (sub1 x)
  (- x 1))



; 加：递归把y递减至0、y递减几次就对x递增几次
(define (o+ x y)
  (cond ((zero? y) x)
	(else (add1 (o+ x (sub1 y))))))

; 减：递归把y递减至0、y递减几次就对x递减几次
(define (o- x y)
  (cond ((zero? y) x)
	(else (sub1 (o- x (sub1 y))))))

; 乘：将x添加自身y次
(define (o* x y)
  (cond ((zero? y) 0)
		(else (o+ x (o* x (sub1 y))))))

; 除：看x能装下多少个y
(define (o/ x y)
  (cond ((o< x y) 0)
		(else (add1 (o/ (o- x y) y)))))

; 幂：将x乘以自身y次
(define (o^ x y)
  (cond ((zero? y) 1)
		(else (o* x (o^ x (sub1 y))))))




; 小于：优先大的值是否为0
(define (o< x y)
  (cond ((zero? y) false)
		((zero? x) true)
		(else (o< (sub1 x) (sub1 y)))))

; 大于：优先大的值是否为0
(define (o> x y)
  (cond ((zero? x) false)
		((zero? y) true)
		(else (o> (sub1 x) (sub1 y)))))

; 大于等于：对x和y递减，如果y比x先到0则说明x > y
(define (o>= x y)
  (cond ((and (zero? x) (not (zero? y))) false)
		((zero? y) true)
		(else (o>= (sub1 x) (sub1 y)))))

; 等于：x和y递减时必须同时为0
(define (o= x y)
  (cond ((and (zero? x) (zero? y)) true)
		((or (zero? x) (zero? y)) false)
		(else (o= (sub1 x) (sub1 y)))))

; 等于：用">" 和"<"实现"="
(define (o== x y)
  (cond ((and (o< x y) (o> x y)) false)
		(else true)))




; 求tup元素中的和
(define (addtup tup)
  (cond ((null? tup) 0)
	(else (o+ (car tup) (addtup (cdr tup))))))

; 求两个tup按位合的tup
(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
		((null? tup2) tup1)
		(else (cons (o+ (car tup1) (car tup2))
					(tup+ (cdr tup1) (cdr tup2))))))

; 计算lat的长度
(define (length lat)
  (cond ((null? lat) 0)
		(else (add1 (length (cdr lat))))))

; 获取lat中第n个元素
(define (pick n lat)
  (cond ((null? lat) '())
		(else (cond ((o= n 0) (car lat))
					(else (pick (sub1 n) (cdr lat)))))))

; 删除lat中的第n个元素
(define (rempick n lat)
  (cond ((null? lat) '())
		(else (cond ((o= n 0) (cdr lat))
					(else (cons (car lat) (rempick (sub1 n) (cdr lat))))))))




; 移除原列表中的所有数字(借用number?函数)
(define (no-nums lat)
  (cond ((null? lat) '())
		(else (cond ((number? (car lat)) (no-nums (cdr lat)))
					(else (cons (car lat) (no-nums (cdr lat))))))))

; 保留列表中所有数字
(define (all-nums lat)
  (cond ((null? lat) '())
		(else (cond ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
					(else (all-nums (cdr lat)))))))

; 比较两个参数是否为同一个原子
(define (eqan? x y)
  (cond ((and (number? x) (number? y)) (= x y))
		((or (number? x) (number? y)) false)
		(else (equal? x y))))

; 统计a在lat中出现的次数
(define (occur a lat)
  (cond ((null? lat) 0)
		(else (cond ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
			  		(else (occur a (cdr lat)))))))
