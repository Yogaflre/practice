#lang racket

; 判断x是否为原子
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

; 判断x是否为算数表达式(数字和符号组成)
(define (numbered? x)
  (cond ((atom? x) (number? x))
		(else (and (numbered? (car x))
				   (numbered? (car (cdr (cdr x))))))))
(numbered? (list 1 "+" (list 2 "*" 3)))

; 使用辅助函数抽象表示方式
; 获取表达式的计算符号
(define (operator x)
  (car x))
; 获取表达式第一个值
(define (1nd-sub-exp x)
  (car (cdr x)))
; 获取表达式第二个值
(define (2nd-sub-exp x)
  (car (cdr (cdr x))))

; 计算算数表达式的值
(define (value x)
  (cond ((atom? x) (cond ((numbered? x) x)
						 (else '())))
		((equal? (operator x) "+") (+ (1nd-sub-exp x)
								 	  (2nd-sub-exp x)))
		((equal? (operator x) "*") (* (1nd-sub-exp x)
								 	  (2nd-sub-exp x)))
		((equal? (operator x) "^") (expt (1nd-sub-exp x)
								 		 (2nd-sub-exp x)))))
(value (list "+" 1 2))

; 数字表示法：使用'()来表示0 ('())表示1 ('() '())表示2 以此类推
(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (+* m n)
  (cond ((sero? m) n)
		(else (edd1 (+* (zub1 m) n)))))
(+* (list '()) (list '() '()))
