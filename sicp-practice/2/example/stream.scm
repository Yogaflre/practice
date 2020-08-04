; Everthing is stream.函数式/响应式编程模型

; 聚合元素
(define (reduce op init items)
    (if (null? items)
        init
        (reduce op
                (op init (car items))
                (cdr items))))
(reduce + 0 (list 1 2 3))


; 枚举出所有树叶
(define (tree-nodes tree)
    (cond ((null? tree) ())
          ((not (pair? tree)) (list tree))
          (else (append (tree-nodes (car tree))
                        (tree-nodes (cdr tree))))))


; 使用stream实现sum-odd-squares(计算奇数叶子节点的平方和)
(define (sum-odd-squares tree)
    (reduce +
            0 
            (map square
                 (filter odd?
                         (tree-nodes tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4) 5)))