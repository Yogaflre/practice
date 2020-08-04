; The exponentiation algorithms in this section are based on performing exponentiation by means of repeated multiplication.
; In a similar way, one can perform integer multiplication by means of repeated addition.
; The following multiplication procedure (in which it is assumed that our language can only add, not multiply) is analogous to the expt procedure:

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(* 2 3)


; This algorithm takes a number of steps that is linear in b.
; Now suppose we include, together with addition, operations double, which doubles an integer, and halve, which divides an (even) integer by 2.
; Using these, design a multiplication procedure analogous to fast-expt that uses a logarithmic number of steps.

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

; 乘法优化
(define (fast-multi a b)
    (cond ((= b 0) 0)
          ((even? b) (double (* a (halve b))))  ; 偶数  2 * 4 = 2*2 + 2*2
          (else (+ a (* a (- b 1))))))  ; 奇数  2 * 3 = 2 + 2*2
(define (even? n)
    (= (remainder n 2) 0))

(fast-multi 2 3)