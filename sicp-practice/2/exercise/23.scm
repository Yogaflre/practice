; The procedure for-each is similar to map. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, for-each just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all—for-each is used with procedures that perform an action, such as printing. For example,
; The value returned by the call to for-each (not illustrated above) can be something arbitrary, such as true. Give an implementation of for-each.
(define (for-each f items)
    (cond ((null? items) "")
          (else (f (car items)) ; if/else中不能调用多个并列的procedure
                (for-each f (cdr items)))))

(for-each 
    (lambda (x) (newline) (display x))
    (list 57 321 88))
; 57
; 321
; 88