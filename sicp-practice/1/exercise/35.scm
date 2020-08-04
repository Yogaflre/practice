; Show that the golden ratio φ (1.2.2) is a fixed point of the transformation x↦1+1/x, 
; and use this fact to compute φ by means of the fixed-point procedure.



(define (fix-point func guess)
    (let ((new_guess (func guess)))
         (if (close-enough? new_guess guess)
             new_guess
             (fix-point func new_guess))))

(define (close-enough? x y)
    (< (abs (- x y)) 0.00001))

(fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0)  ; 1.6180327868852458