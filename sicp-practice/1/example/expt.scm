; 求幂

(define (expt b n)
    (if (<= n 1)
        (* b n)
        (* b (expt b (- n 1)))))
(expt 2 3)

; liner
(define (expt-iter b n)
    (iter b n 1))
(define (iter b n tmp)
    (if (<= n 1)
        (* tmp b)
        (iter b (- n 1) (* tmp b))))
(expt-iter 2 3)

; fast-expt
; b^2 = b * b
; b^4 = b^2 * b^2
; b^8 = b^4 * b^4
(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))
(define (even? n)
    (= (remainder n 2) 0)) ; remainder是取余运算
(fast-expt 2 4)