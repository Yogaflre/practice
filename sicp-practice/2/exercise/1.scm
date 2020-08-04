; Define a better version of make-rat that handles both positive and negative arguments.
; Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.


(define (gcd a b)
    (let ((a (abs a))
          (b (abs b)))
         (if (= b 0)
             a
             (gcd b (remainder a b)))))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat a b)
    (let ((g (gcd a b)))
         (let ((a (/ a g))
               (b (/ b g)))
              (if (positive? b)
                  (cons a b)
                  (cons (- a) (- b))))))

(make-rat 1 2)
(make-rat -1 -2)
(make-rat -1 2)
(make-rat 1 -2)