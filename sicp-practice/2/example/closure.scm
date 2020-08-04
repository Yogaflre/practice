; 闭包性质：组合式的成员本身还可以是组合式


; list：cons的组合
(define items (list 1 2 3 4))

(car items) ; 1
(cdr items) ; (2 3 4)

; 获取list目标位置元素
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))
(list-ref items 0)
(list-ref items 3)
(list-ref items 4)

; 获取长度
(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))
(length items)


; 合并两个list
(define items2 (list 5 6))

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))
(append items items2)


; Mapping映射
(define (map f items)
    (if (null? items)
        '() ; scheme取消“nil”，由()或‘()来表示空
        (cons (f (car items)) (map f (cdr items)))))

(map (lambda (x) (* x 10)) items)