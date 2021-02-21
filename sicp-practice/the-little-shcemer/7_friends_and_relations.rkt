#lang racket

; 检测a是否在l中(its_full_of_stars.scm)
(define (member? a l)
  (cond ((null? l) false)
		((list? (car l)) (or (member? a (car l)) (member? a (cdr l))))
		(else (cond ((equal? a (car l)) true)
					(else (member? a (cdr l)))))))

; 校验是否为set，以此对比每一个元素
(define (set? lat)
  (cond ((null? lat) true)
		((member? (car lat) (cdr lat)))
		(else (set? (cdr lat)))))




(define test-lat (list "apple" "peach" "pear" "peach" "plum" "apple" "lemon" "peach"))

; 将list变为set
(define (makeset lat)
  (cond ((null? lat) '())
		((member? (car lat) (cdr lat)) (makeset (cdr lat)))
		(else (cons (car lat) (makeset (cdr lat))))))
(makeset test-lat)


; 移除lat中的所有a
(define (multirember a lat)
  (cond ((null? lat) '())
		((equal? a (car lat)) (multirember a (cdr lat)))
		(else (cons (car lat) (multirember a (cdr lat))))))

; 使用multirember实现makeset
(define (makeset2 lat)
  (cond ((null? lat) '())
		(else (cons (car lat)
					(makeset2 (multirember (car lat) (cdr lat)))))))
(makeset2 test-lat)




; 判断set2是否包含set1
(define (subset? set1 set2)
  (cond ((null? set1) true)
		((member? (car set1) set2) (subset? (cdr set1) set2))
		(else false)))
(subset? (list 5 "chicken" "wings") (list 5 "hamburgers" 2 "pieces" "fried" "wings" "chicken"))

; 判断两个set是否想等
(define (eqset? set1 set2)
  (and (subset? set1 set2)
	   (subset? set2 set1)))
(eqset? (list 5 "chicken" "wings") (list "wings" 5 "chicken"))

; 判断set1中至少有一个元素在set2中 
(define (intersect? set1 set2)
  (cond ((null? set1) false)
		(else (or (member? (car set1) set2)
				  (intersect? (cdr set1) set2)))))
(intersect? (list "stewed" "tomatoes" "and" "macaroni") (list "macaroni" "and" "cheese"))

; 取两个set交集
(define (intersect set1 set2)
  (cond ((null? set1) '())
		((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
		(else (intersect (cdr set1) set2))))
(intersect (list "stewed" "tomatoes" "and" "macaroni") (list "macaroni" "and" "cheese"))

; 取两个set并集。遍历set1，添加set2中不存在的元素，set1为空时返回set2
(define (union set1 set2)
  (cond ((null? set1) set2)
		((member? (car set1) set2) (union (cdr set1) set2))
		(else (cons (car set1) (union (cdr set1) set2)))))
(union (list "macaroni" "and" "cheese") (list "stewed" "tomatoes" "and" "macaroni" "casserole"))

; 返回所有set1存在，set2不存在的元素
(define (xxx set1 set2)
  (cond ((null? set1) '())
		((member? (car set1) set2) (xxx (cdr set1) set2))
		(else (cons (car set1) (xxx (cdr set1) set2)))))
(xxx (list "macaroni" "and" "cheese") (list "stewed" "tomatoes" "and" "macaroni" "casserole"))

; 求所有list的公共值(l-set不能为null)
(define (intersectall l-set)
  (cond ((null? (cdr l-set)) (car l-set))
		(else (intersect (car l-set) (intersectall (cdr l-set))))))
(intersectall (list (list "a" "b" "c") (list "c" "a" "d" "e") (list "e" "f" "a")))




(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

; 判断list是否是pair? (包含两个元素的list为pair)
(define (a-pair? l)
  (cond ((null? l) false)
		((atom? l) false)	
		((null? (cdr l)) false)
		((null? (cdr (cdr l))) true)
		(else false)))

; 取每个集合中的第一个元素
(define (firsts l)
  (cond ((null? l) '())
		(else (cons (car (car l))
					 (firsts (cdr l))))))
(firsts (list (list "a" "b") (list "c" "d") (list "e" "f")))

; 判断pair集合中的第一个元素是不是set
(define (fun? rel)
  (set? (firsts rel)))

; 反转pair
(define (revpair pair)
  (list (car (cdr pair)) (car pair)))

; 反转rel中的每个parl
(define (revrel rel)
  (cond ((null? rel) '())
		(else (cons (revpair (car rel))
					(revrel (cdr rel))))))
(revrel (list (list 8 "a") (list "pumpkin" "pie") (list "got" "sick")))

; 判断pair集合中第二个元素是不是set
(define (fullfun? rel)
  (fun? (revrel rel)))
