; Exercise 2.63: Each of the following two procedures converts a binary tree to a list.

; 取节点元素
(define (entry tree) (car tree))
; 取左子树
(define (left-branch tree) (cadr tree))
; 取右子树
(define (right-branch tree) (caddr tree))
; 构造节点(为什么用list不用原生cons？)
(define (make-tree entry left right) (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define tree (make-tree 1 (make-tree 2 () ()) (make-tree 3 () ())))
; a. Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?
(tree->list-1 tree)
(tree->list-2 tree)
; 这两个procedure生成的结果相同(2 1 3)，都是中序遍历。图2.16的中序遍历表示(1 3 5 7 9 11)


; b. Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with n elements to a list? If not, which one grows more slowly?
; tree->list-2复杂度更低一些，cons比append时间复杂度更低