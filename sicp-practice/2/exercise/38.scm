; The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


; What are the values of

(fold-right / 1 (list 1 2 3))   ; 3/2
(fold-left  / 1 (list 1 2 3))   ; 1/6
(fold-right list () (list 1 2 3))   ; (1 (2 (3 ())))
(fold-left  list () (list 1 2 3))   ; (((() 1) 2) 3)

; fold-right = (right op left)
; fold-left  = (left op right)
; 如果要使fold-left和fold-right产生相同的结果，那么op操作必须满足交换律(a op b = b op a)