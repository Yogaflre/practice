; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a logarithmic number of steps.

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))


(define (fast-multi-iter a b)
  (iter a b 0))
; 将b的乘数逐渐转移给a
; 2 * 4 = 4 * 2
;       = 8 * 1
; 2 * 3 = 2 * 2 + 2  在奇数时需要tmp字段来表示附加数
;       = 4 * 1 + 2
(define (iter a b tmp)
  (cond ((= b 0) tmp)
        ((even? b) (iter (double a) (halve b) tmp))
        (else (iter a (- b 1) (+ tmp a)))))


(fast-multi-iter 3 4)
(fast-multi-iter 4 4)
(fast-multi-iter 4 0)