#lang racket

; 获取lat中第n个元素(从1开始)
(define (pick n lat)
  (cond ((null? lat) '())
		(else (cond ((= n 1) (car lat))
					(else (pick (- n 1) (cdr lat)))))))
; 按索引位置查找元素
(define (keep-looking a n lat)
  (cond ((number? n) (keep-looking a (pick n lat) lat))
		(else (equal? a n))))
(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))
(looking "caviar" (list 6 2 4 "caviar" 5 7 3))

; 偏函数(无限递归)
(define (recursive x)
  (recursive x))


(define p (list (list 1 2) (list 3 4)))

; 将pair中第一个元素的值依次右移合并到pair第二个元素
(define shift (lambda (pair)
				(list (first (first pair)) 
					  (list (second (first pair))
							(second pair)))))
(shift p)


(define (atom? x)
  (and (not (pair? x)) (not (null? x))))
; 判断list是否是pair? (包含两个元素的list为pair)
(define (a-pair? l)
  (cond ((null? l) false)
		((atom? l) false)	
		((null? (cdr l)) false)
		((null? (cdr (cdr l))) true)
		(else false)))

; 统计原子数量
(define (length* pora)
  (cond ((atom? pora) 1)
		(else (+ (length* (first pora))
				 (length* (second pora))))))
(length* p)


(define (align pora)
  (cond ((atom? pora) pora)
		((a-pair? (first pora)) (align (shift pora)))
		(else (list (first pora)
					(align (second pora))))))
(align p)



; Collatz猜想(partial function): 对任意一个奇数*3、对任意一个偶数减半。则会无限循环下去(n = 1时可以跳出循环)
(define (collatz n)
  (cond ((= n 1) 1)
		((even? n) (collatz (/ n 2)))
		(else (collatz (+ 1 (* 3 n))))))
(collatz 10)

; Ackermann函数(total function): 非原始递归函数
(define (ackermann n m)
  (cond ((zero? n) (+ m 1))
		((zero? m) (ackermann (- n 1) 1))
		(else (ackermann (- n 1) (ackermann n (- m 1))))))
(ackermann 1 2)
; (ackermann 4 3) 沉浸在计算中很难返回正确答案了



; 停机问题: 我们无法定义一个函数来判断这个函数是否会有返回值
(define (will-stop?)
  (display "unknown"))





; Y-Combinator: Anonymous recursion
; 正常情况下定义length函数实现递归
(define length (lambda (l)
				 (cond ((null? l) 0)
					   (else (+ 1 (length (cdr l)))))))
; 定义一个不会停止的辅助函数(永远也不会被执行)
(define (? l)
  (? l))
; 1.去除define定义，创建匿名函数。最后一行由于未知函数名，所以暂时用"?"代替
; 计算l为空的list长度。由于“?”是未知函数，所以该函数只能求空list长度
(lambda (l)
  (cond ((null? l) 0)
		(else (+ 1 (? (cdr l))))))
; 计算长度为1的list。如果要计算更大的长度则需要不断写l的重复代码
(lambda (l)
  (cond ((null? l) 0)
		(else (+ 1 ((lambda (l)
					  (cond ((null? l) 0)
							(else (+ 1 (? (cdr l))))))
					(cdr l))))))
; 2.抽象入参为l的匿名函数，把"?"提取出来变成入参len
((lambda (len)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (+ 1 (len (cdr l)))))))
 ?)
; 计算长度为1的list，把len匿名函数自身带入"?"中。类似于((func (func ?)) (list 1))的嵌套结构
((lambda (len)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (+ 1 (len (cdr l)))))))
 ((lambda (len)
	(lambda (l)
	  (cond ((null? l) 0)
			(else (+ 1 (len (cdr l)))))))
  ?))
; 3.继续抽象参数为len的匿名函数
((lambda (mk-length)
   (mk-length ?))
 (lambda (len)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (+ 1 (len (cdr l))))))))
; 计算长度为1的list，把mk-length的值带入mk-length中（递增mk-length即可）
((lambda (mk-length)
   (mk-length
	 (mk-length ?)))
 (lambda (len)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (+ 1 (len (cdr l))))))))
; 4.由于最深层递归l=null，所以"?"函数就不重要了。我们可以用任意函数替换它（用mk-length自身替换）；len只是参数名，也可以用mk-length替换
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (+ 1 (mk-length (cdr l))))))))
; 当list>0时有问题，mk-length的入参为函数，所以最后一步不能接受一个list作为入参。可以递归mk-length函数，因为递归到最后mk-length函数要传入list
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (+ 1 ((mk-length mk-length) (cdr l)))))))) ; mk-length自己递归自己
; 5.(mk-length mk-length)看起来不像求l长度的函数，我们可以用len作为入参(传入(mk-length mk-length))进一步抽象
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (len)
	  (lambda (l)
		(cond ((null? l) 0)
			  (else (+ 1 (len (cdr l)))))))
	(mk-length mk-length))))
; 把(mk-length mk-length)提取出之后，则会在入参时无限递归，再也不会返回一个接收list的函数。可以把(mk-length mk-length)构造为一个单参数函数
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (len)
	  (lambda (l)
		(cond ((null? l) 0)
			  (else (+ 1 (len (cdr l)))))))
	(lambda (x)
	  ((mk-length mk-length) x)))))
; 6.提取入参为len的函数并赋予一个名字le
((lambda (le)
   ((lambda (mk-length)
	  (mk-length mk-length))
	(lambda (mk-length)
	  (le (lambda (x)
			((mk-length mk-length) x))))))
 (lambda (len)
   (lambda (l)
	 (cond ((null? l) 0)
		   (else (+ 1 (len (cdr l))))))))
; 7.入参为len的函数已经和我们最初的mk-length函数一样了。我们可以提取出入参为le的函数作为匿名函数递归模板
(lambda (le)
   ((lambda (mk-length)
	  (mk-length mk-length))
	(lambda (mk-length)
	  (le (lambda (x)
			((mk-length mk-length) x))))))
; 简化参数名，并给这个递归匿名函数的函数一个最终的名字: Y-Combinator
(define Y
  (lambda (le)
   ((lambda (f) (f f))
	(lambda (f)
	  (le (lambda (x) ((f f) x)))))))


((Y (lambda (len)
	  (lambda (l)
		(cond ((null? l) 0)
			  (else (+ 1 (len (cdr l))))))))
 (list 1 2))
