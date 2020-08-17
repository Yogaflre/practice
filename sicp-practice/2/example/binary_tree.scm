; 构造二叉搜索树

; 取节点元素
(define (entry tree) (car tree))
; 取左子树
(define (left-branch tree) (cadr tree))
; 取右子树
(define (right-branch tree) (caddr tree))
; 构造节点(为什么用list不用原生cons？)
(define (make-tree entry left right) (list entry left right))


(define tree (make-tree 1 (make-tree 2 () ()) (make-tree 3 () ())))


(define (element-of-set? x tree)
    (if (null? tree)
        false
        (let ((v (entry tree)))
             (cond ((= x v) true)
                   ((< x v) (element-of-set? x (left-branch tree)))
                   ((> x v) (element-of-set? x (right-branch tree)))))))
(element-of-set? 3 tree)


(define (adjoin-tree x tree)
    (cond ((null? tree) (make-tree x () ()))
          ((= (entry tree) x) tree)
          ((> (entry tree) x) (make-tree (entry tree)
                                         (adjoin-tree x (left-branch tree))
                                         (right-branch tree)))
          ((< (entry tree) x) (make-tree (entry tree)
                                         (left-branch tree)
                                         (adjoin-tree x (right-branch tree))))))
(adjoin-tree 4 tree)
