; Newton’s method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value
; (x/y2+2y)/3
; Use this formula to implement a cube-root procedure analogous to the square-root procedure. 
; (In 1.3.4 we will see how to implement Newton’s method in general as an abstraction of these square-root and cube-root procedures.)



(define (square x)
    (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

; solution
(define (improve guess x) 
  (/ ((+ (/ x (square guess)) (* 2 guess))) 3))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 8)