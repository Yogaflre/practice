; Implement a representation for rectangles in a plane. (Hint: You may want to make use of Exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle.
; Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?

(define (make-rectangle len wid)
    (cons len wid))

(define (rectangle-len rectangle)
    (car rectangle))

(define (rectangle-wid rectangle)
    (cdr rectangle))

(define (perimeter rectangle)
    (+ (* 2 (rectangle-len rectangle))
       (* 2 (rectangle-wid rectangle))))

(define (area rectangle)
    (* (rectangle-len rectangle) (rectangle-wid rectangle)))


(define rectangle (make-rectangle 2 4))

(perimeter rectangle)
(area rectangle)