; Redefine count-leaves from 2.2.2 as an accumulation:

(define (accumulate op init items)
    (if (null? items)
        init
        (op (car items)
            (accumulate op
                        init
                        (cdr items)))))

; TODO 搞明白(map里为什么只递归t就可以计算count值，在哪里递归了左右子树？)
(define (count-leaves t)
  (accumulate +
              0 
              (map (lambda (tree)
                           (if (pair? tree)
                               (count-leaves tree)
                               1))
                   t)))

(count-leaves (list 1 (list 2 (list 3 4) 5)))