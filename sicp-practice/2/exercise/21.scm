; The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers.
; Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:

; 自定义实现map过程
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

; scheme自带map过程
(define (square-list items)
  (map (lambda (x) (square x)) items))

(square-list (list 1 2 3 4))    ; (1 4 9 16)