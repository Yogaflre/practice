(define (average x y)
    (/ (+ x y) 2))
(define (close-enough? x y)
    (< (abs (- x y)) 0.001))


; 1.通过区间折半寻找方程的根
(define (search f negative positive)
    (let ((mid (average positive negative)))
         (if (close-enough? positive negative)
             mid
             (let ((mid-val (f mid)))
                  (cond ((positive? mid-val) (search f negative mid))
                        ((negative? mid-val) (search f mid positive))
                        (else mid))))))
; 校验 f(a)/f(b) 的正负性
(define (half-interval f a b)
    (let ((a-val (f a))
          (b-val (f b)))
         (cond ((and (negative? a-val) (positive? b-val)) (search f a b))
               ((and (negative? b-val) (positive? a-val)) (search f b a))
               (else (error "Values are not of opposite sign" a b)))))

(half-interval sin 2.0 4.0)     ; 3.14111328125
(half-interval (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)     ; 1.89306640625




; 2.寻找函数的不动点，即 f(x)=x。参见1/exercise/35.scm
; 将 f(guess)的值当作新的入参，直到找到最接近的值