; Modify fixed-point so that it prints the sequence of approximations it generates, 
; using the newline and display primitives shown in Exercise 1.22. 
; Then find a solution to xx=1000 by finding a fixed point of x↦log(1000)/log(x). 
; (Use Scheme’s primitive log procedure, which computes natural logarithms.) 
; Compare the number of steps this takes with and without average damping. 
; (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1)=0.)



(define (fixed-point func guess)
    (newline)
    (display guess)
    (let ((new_guess (func guess)))
         (if (close-enough? new_guess guess)
             new_guess
             (fixed-point func new_guess))))

(define (close-enough? x y)
    (< (abs (- x y)) 0.00001))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
