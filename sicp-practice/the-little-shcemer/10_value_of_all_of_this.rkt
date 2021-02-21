#lang racket

; 构建新的entry
(define new-entry
  (lambda (ks vs)
	(cons ks (cons vs '()))))
(define get-ns
  (lambda (entry)
	(car entry)))
(define get-vs
  (lambda (entry)
	(car (cdr entry))))

; 定义测试用例
(define ns '(appetizer entree bevarage))
(define vs '(pate boeuf vin))
(define entry1 (new-entry ns vs))
(define entry2 (new-entry '(entree dessert) '(spaghetti spumoni)))

; 根据key查找entry中的值
(define lookup-in-entry
  (lambda (name entry entry-f)
	(lookup-in-entry-help name (car entry) (car (cdr entry)) entry-f)))
(define lookup-in-entry-help
  (lambda (name ns vs entry-f)
	(cond ((null? ns) (entry-f name))
		  ((equal? (car ns) name) (car vs))
		  (else (lookup-in-entry-help name (cdr ns) (cdr vs) entry-f)))))
(lookup-in-entry 'entree entry1 (lambda (name) '()))


; 构建entry到table中
(define extend-table cons)

; 根据key查找table中的值。table-f用来处理找不到key的情况，所以把table的下一个entry作为入参递归到table-f中
(define lookup-in-table
  (lambda (name table table-f)
	(cond ((null? table) (table-f name))
		  (else (lookup-in-entry name
								 (car table)
								 (lambda (name)
								   (lookup-in-table name
													(cdr table)
													table-f)))))))
; 定义测试用例
(define table1
  (extend-table entry1 (extend-table entry2 '())))
(lookup-in-table 'dessert table1 (lambda (name) '()))





; <解释器>
; 辅助函数
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))
(define (add1 x)
  (+ x 1))
(define (sub1 x)
  (- x 1))
(define build
  (lambda (s1 s2)
	(cons s1 (cons s2 '()))))
(define first
  (lambda (p)
	(car p)))
(define second
  (lambda (p)
	(car (cdr p))))
(define third
  (lambda (p)
	(car (cdr (cdr p)))))


; 在lisp语言中有常见的6中类型：*const *list *identifier *lambda *cond *application
; 实现各种类型
; 实现*const类型
(define *const
  (lambda (e table)
	(cond ((number? e) e)
		  ((eq? e #t) #t)
		  ((eq? e #f) #f)
		  (else (build (quote primtive) e)))))

; 实现*quote类型
(define text-of second)
(define *quote
  (lambda (e table)
	(text-of e)))


; 实现*indentifier
(define initial-table
  (lambda (name)
	(car (quote ()))))
(define *identifier
  (lambda (e table)
	(lookup-in-table e table initial-table)))

; 实现*lambda：(non-primitive ((((y z) ((8) 9))) (x) (cons x y)))
(define table-of first) ; 获取实参数
(define formals-of second) ; 获取形参
(define body-of third) ; 获取函数体
(define *lambda
  (lambda (e table)
	(build (quote non-primitive)
		   (cons table (cdr e)))))

; 实现*cond
; 解释else规则
(define else?
  (lambda (x)
	(cond ((atom? x) (eq? x (quote else)))
		  (else #f))))
; 获取cond中判断语句
(define question-of first)
; 获取cond中结果语句
(define answer-of second)
; 解释cond其中一行
(define evcon
  (lambda (lines table)
	(cond ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
		  ((meaning (question-of (car lines) table)) (meaning (answer-of (cdr lines) table)))
		  (else (evcon (cdr lines))))))
; 获取cond的判断list
(define cond-lines-of cdr)
(define *cond
  (lambda (e table)
	(cond (evcon (cond-lines-of e) table))))


; 实现*application
; 需要找出函数主体
(define function-of car)
; 找出参数列表
(define arguments-of cdr)
; 应用一个函数前，获取每个参数的结果列表
(define evlis
  (lambda (args table)
	(cond ((null? args) (quote ()))
		  (else (cons (meaning (car args) table)
					  (evlis (cdr args) table))))))
; 判断是原始函数还是非原始函数
(define primitive?
  (lambda (l)
	(eq? (first l) (quote primitive))))
(define non-primitive?
  (lambda (l)
	(eq? (first l) (quote non-primitive))))
; 应用函数
(define apply-primitive
  (lambda (name vals)
	(cond ((eq? name (quote cons)) (cons (first vals) (second vals)))
		  ((eq? name (quote car)) (car (first vals)))
		  ((eq? name (quote cdr)) (cdr (first vals)))
		  ((eq? name (quote null?)) (null? (first vals)))
		  ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
		  ((eq? name (quote atom?)) (atom? (first vals)))
		  ((eq? name (quote zero?)) (zero? (first vals)))
		  ((eq? name (quote add1)) (add1 (first vals)))
		  ((eq? name (quote sub1)) (sub1 (first vals)))
		  ((eq? name (quote number?)) (number? (first vals))))))
(define apply-closure
  (lambda (closure vals)
	(meaning (body-of closure)
			 (extend-table (new-entry (formals-of closure) vals)
						   (table-of closure)))))
(define apply
  (lambda (fun vals)
	(cond ((primitive? fun) (apply-primitive (second fun) vals))
		  ((non-primitive? fun) (apply-closure (second fun) vals)))))
(define *application
  (lambda (e table)
	(apply (meaning (function-of e) table)
		   (evlis (arguments-of e) table))))


; 解释e的动作，为每种S-表达式都产生正确的action
; 识别原子类型
(define atom-to-action
  (lambda (e)
	(cond ((number? e) *const)
		  ((eq? e #t) *const)
		  ((eq? e #f) *const)
		  ((eq? e (quote cons)) *const)
		  ((eq? e (quote car)) *const)
		  ((eq? e (quote cdr)) *const)
		  ((eq? e (quote null?)) *const)
		  ((eq? e (quote eq?)) *const)
		  ((eq? e (quote atom?)) *const)
		  ((eq? e (quote zero?)) *const)
		  ((eq? e (quote add1)) *const)
		  ((eq? e (quote sub1)) *const)
		  ((eq? e (quote number?)) *const)
		  (else *identifier))))
; 识别非原子类型
(define list-to-action
  (lambda (e)
	(cond ((atom? (car e)) (cond ((eq? (car e) (quote quote)) *quote)
								 ((eq? (car e) (quote lambda)) *lambda)
								 ((eq? (car e) (quote cond)) *cond)
								 (else *application)))
		  (else *application))))
(define expression-to-action
  (lambda (e)
	(cond ((atom? e) (atom-to-action e))
		  (else (list-to-action e)))))

(define meaning
  (lambda (e table)
	((expression-to-action e) e table)))

; 解释e
(define value
  (lambda (e)
	(meaning e (quote ()))))

