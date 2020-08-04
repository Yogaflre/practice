; Observe that our model of evaluation allows for combinations whose operators are compound expressions. 
; Use this observation to describe the behavior of the following procedure

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))



; define a "a-plus-abs-b" function. receive two params.
; if "b>0", return a + b. if "b<=0" return a - b.