; 树形递归

; fibonacci
(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))

(fib 4) ;3

; fibonacci优化为线性递归
(define (fib-iter n)
    (iter 0 1 n))

(define (iter a b n)
    (cond ((= 0 n) a)
          (else (iter b (+ a b) (- n 1)))))

(fib-iter 4) ;3



; counting change
(define (count-change amount)
    (cc amount 5))

; 递归当前金额和非当前金额两种情况
(define (cc amount kinds)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds 0)) 0)
          (else (+  (cc amount (- kinds 1))
                    (cc (- amount (change kinds)) kinds)))))

(define (change kind)
    (cond ((= kind 1) 1)
          ((= kind 2) 5)
          ((= kind 3) 10)
          ((= kind 4) 25)
          ((= kind 5) 50)))

(count-change 100)  ; 292
(count-change 11)  ; 4