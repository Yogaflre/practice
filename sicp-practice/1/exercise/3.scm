; Exercise 1.3: Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.



; Fool solution.
(define (sum a b c) (cond ((> a b) (if (> b c)
                                       (+ a b)
                                       (+ a c)))
                          ((> b a) (if (> a c)
                                       (+ b a)
                                       (+ b c)))
                          ((= a b) (if (> a c)
                                       (+ a b)
                                       (+ a c)))))

(sum 2 5 1)
; TODO: Try to write better solution.