#lang racket


; 柯里化(currying)
(define eq?-c (lambda (a)
				(lambda (x)
				  (eq? a x))))
((eq?-c 1) 1)

; 实现删除第一个元素(currying)
(define rember-f (lambda (test?)
				   (lambda (a l)
					 (cond ((null? l) '())
						   ((test? a (car l)) (cdr l))
						   (else (cons (car l)
									   ((rember-f test?) a (cdr l))))))))
((rember-f eq?) 5 (list 6 2 5 3))



(define lat (list "B" "B" "C"))
; 公共方法: 前置/后置/替换旧元素(currying)
(define insert-f (lambda (build new old l)
					(cond ((null? l) '())
						  ((list? (car l)) (cons (insert-f build new old (car l)) (insert-f build new old (cdr l))))
						  (else (cond ((equal? old (car l)) (build new old (insert-f build new old (cdr l))))
									  (else (cons (car l) (insert-f build new old (cdr l)))))))))
(define seqL (lambda (x y l)
			   (cons x (cons y l))))
(define seqR (lambda (x y l)
			   (cons y (cons x l))))
(define seqS (lambda (x y l)
			   (cons x l)))
(define seqD (lambda (x y l)
			   l))
; insertL: 添加到左边
(insert-f seqL "A" "B" lat) 
; insertR: 添加到右边
(insert-f seqR "A" "B" lat)
; subset: 替换该元素
(insert-f seqS "A" "B" lat)
; 使用公共方法实现删除元素
(insert-f seqD false "B" lat)




(define (atom? x)
  (and (not (pair? x)) (not (null? x))))
(define (numbered? x)
  (cond ((atom? x) (number? x))
		(else (and (numbered? (car x))
				   (numbered? (car (cdr (cdr x))))))))
(define (operator x)
  (car x))
(define (1nd-sub-exp x)
  (car (cdr x)))
(define (2nd-sub-exp x)
  (car (cdr (cdr x))))
(define (atom-to-func x)
  (cond ((equal? "+" x) +)
		((equal? "*" x) *)
		(else expt)))
; 利用函数抽象重写value方法
(define (value x)
  (cond ((atom? x) (cond ((numbered? x) x)
						 (else '())))
		(else ((atom-to-func (operator x)) (1nd-sub-exp x) (2nd-sub-exp x)))))
(value (list "+" 1 2))



; 将new插入到oldL的左边和oldR的右边(oldL和oldR是不同原子)
(define (multiinsertLR new oldL oldR lat)
  (cond ((null? lat) '())
		((equal? oldL (car lat)) (seqL new oldL (multiinsertLR new oldL oldR (cdr lat))))
		((equal? oldR (car lat)) (seqR new oldR (multiinsertLR new oldL oldR (cdr lat))))
		(else (cons (car lat) (multiinsertLR new oldL oldR lat)))))
(multiinsertLR "W" "B" "C" lat)


(define (add1 x)
  (+ x 1))
; 划重点: 比multiinsertLR多传一个col函数，用于统计左右各插入了多少次
(define (multiinsertLR&co new oldL oldR lat col)
  (cond ((null? lat) (col '() 0 0))
		((equal? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat)
												   (lambda (newlat L R)
													 (col (seqL new oldL newlat) (add1 L) R))))
		((equal? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat)
												   (lambda (newlat L R)
													 (col (seqR new oldR newlat) L (add1 R)))))
		(else (multiinsertLR&co new oldL oldR (cdr lat)
								(lambda (newlat L R)
								  (col (cons (car lat) newlat) L R))))))
(multiinsertLR&co "W" "B" "C" lat (lambda (lat L R)
									(list lat L R)))




; 移除所有的奇数
(define l (list (list 1 2) 3 (list 4 5)))
(define (evens-only* l)
  (cond ((null? l) '())
		((atom? (car l)) (cond ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
							   (else (evens-only* (cdr l)))))
		(else (cons (evens-only* (car l)) (evens-only* (cdr l))))))
(evens-only* l)

; 增加collecotr函数分别收集奇偶数
(define (evens-only*&co l col)
  (cond ((null? l) (col '() '()))
		((atom? (car l)) (cond ((even? (car l)) (evens-only*&co (cdr l) (lambda (O E) (col O (cons (car l) E)))))
							   (else (evens-only*&co (cdr l) (lambda (O E) (col (cons (car l) O) E))))))
		(else (evens-only*&co (car l) (lambda (O1 E1)
										(evens-only*&co (cdr l) (lambda (O2 E2)
																  (col (append O1 O2) (append E1 E2)))))))))
(evens-only*&co l (lambda (O E) (list O "|" E)))
