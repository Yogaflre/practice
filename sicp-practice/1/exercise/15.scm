; The sine of an angle (specified in radians) can be computed by making use of the approximation sinx≈x if x is sufficiently small, and the trigonometric identity
; sinx=3sinx3−4sin3x3


(define (cube x)
    (* x x x))

(define (p x)
        (display "!")
        (- (* 3 x) (* 4 (cube x))))

(define (sine x)
    (if (<= (abs x) 0.1)
        x
        (p (sine (/ x 3.0)))))

(sine 1)
(sine 2)
(sine 4)
(sine 8)
(sine 16)
(sine 32)