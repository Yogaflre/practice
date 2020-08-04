; 最大公约数(GCD)

; 欧几里得算法：GCD(a,b) = GCD(b,r)
; 如果r是a除以b的余数，则a和b的公约数也等于b和r的公约数
; GCD(206,40) = GCD(40,6)
;             = GCD(6,4)
;             = GCD(4,2)
;             = GCD(2,0)
;             = 2

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(gcd 206 40)