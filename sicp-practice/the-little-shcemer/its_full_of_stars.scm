; test example
(define test (list (list "how" "much" (list "wood"))
				   "could"
				   (list (list "a" (list "wood") "chuck"))
				   (list "if" (list "a") (list (list "wood" "chuck")))
				   "could"
				   "chuck"
				   "wood"))

; 删除l中的a元素
(define (rember* a l)
  (cond ((null? l) ())
		((list? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
		(else (cond ((equal? a (car l)) (rember* a (cdr l)))
					(else (cons (car l) (rember* a (cdr l))))))))
(rember* "wood" test) 

; 在old后面添加new元素
(define (insertR* new old l)
  (cond ((null? l) ())
		((list? (car l)) (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
		(else (cond ((equal? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
					(else (cons (car l) (insertR* new old (cdr l))))))))
(insertR* "roast" "chuck" test)

; 在old前面添加new元素
(define (insertL* new old l)
  (cond ((null? l) ())
		((list? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
		(else (cond ((equal? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
					(else (cons (car l) (insertL* new old (cdr l))))))))
(insertL* "roast" "chuck" test)

; 统计l中a的个数
(define (occur* a l)
  (cond ((null? l) 0)
		((list? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
		(else (cond ((equal? a (car l)) 1)
					(else (occur* a (cdr l)))))))
(occur* "wood" test)

; 用new替换old
(define (subst* new old l)
  (cond ((null? l) ())
		((list? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
		(else (cond ((equal? (car l) old) (cons new (subst* new old (cdr l))))
					(else (cons (car l) (subst* new old (cdr l))))))))
(subst* "under" "wood" test)

; 判断a是否在l中
(define (member* a l)
  (cond ((null? l) false)
		((list? (car l)) (or (member* a (car l)) (member* a (cdr l))))
		(else (cond ((equal? a (car l)) true)
					(else (member* a (cdr l)))))))
(member* "chuck" test)

; 寻找非空列表最左边非空元素
(define (leftmost l)
  (cond ((list? (car l)) (leftmost (car l)))
		(else (car l))))
(leftmost test)

; 判断两个list是否想等
(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) true)
		((or (null? l1) (null? l2)) false)
		(else (cond ((and (list? (car l1)) (list? (car l2))) (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
					((or (list? (car l1)) (list? (car l2))) false)
					((equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
					(else false)))))
(eqlist? (list "beef" (list (list "sausage")) (list "and" (list "soda"))) 
		 (list "beef" (list (list "sausage")) (list "and" (list "soda"))))
