; The sum procedure above generates a linear recursion.
; The procedure can be rewritten so that the sum is performed iteratively.
; Show how to do this by filling in the missing expressions in the following definition:


; 转换为线性递归，使用result存储中间结果
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 3)    ; 6