; 无序不重复列表

; 判断set中是否存在x元素
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))
(element-of-set? 1 (list 1 2)) ;true

; 为set添加元素
(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))
(adjoin-set 1 (list 2 3))   ; (1 2 3)

; 取两个set交集
(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2) (cons (car set1)
                                                   (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))
(intersection-set (list 1 2 3) (list 2 3 4))    ; (2 3)


; 有序不重复列表

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((= (car set) x) true)
          ((< (car set) x) false)
          (else (element-of-set? x (cdr set)))))
(element-of-set? 2 (list 1 3)) ; false


(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        ()
        (let ((a (car set1))
              (b (car set2)))
             (cond ((= a b) (cons a (intersection-set (cdr set1) (cdr set2))))
                   ((> a b) (intersection-set set1 (cdr set2)))
                   ((< a b) (intersection-set (cdr set1) set2))))))
(intersection-set (list 1 2 3) (list 2 3 4))    ; (2 3)