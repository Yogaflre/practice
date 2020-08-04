; The following pattern of numbers is called Pascal’s triangle.
;          1
;        1   1
;      1   2   1
;    1   3   3   1
;  1   4   6   4   1
;        . . .
; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.
; Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.



; get (x,y)
(define (value row index)
    (cond ((or (= 1 row) (= 1 index) (= row index)) 1)
          (else (+ (value (-1+ row) (-1+ index)) (value (-1+ row) index)))))

; get row
(define (pascal-row row)
    (define (loop n)
        (display (value row n))
        (display " ")
        (cond ((< n row) (loop (+ n 1)))))
    (loop 1))

; get triangle
(define (pascal row)
    (define (loop n)
        (newline)
        (pascal-row n)
        (cond ((< n row) (loop (+ 1 n)))))
    (loop 1))


(pascal 5)