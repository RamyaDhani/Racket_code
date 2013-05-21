#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])


; ; ( parse an−ae ) −> AE
; ; an−ae : sexp

(define (parse sexp)
  
  (if (or (number? sexp) (list? sexp))
  
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp)     
     
     (case (first sexp)
       [(plus) (if (= (length sexp) 3) (add (parse (second sexp))
                 (parse (third sexp))) (error "Expression not in proper format"))]
       [(minus) (if (= (length sexp) 3) (sub (parse (second sexp))
                 (parse (third sexp))) (error "Expression not in proper format"))]
       
       [else (error  "Only plus,minus or number in proper format is allowed")])
     ])
  
  (error "Expression should be number or AE"))
  
  )


(test (parse '3) (num 3))
(test  (parse '(plus 3 4)) (add (num 3) (num 4)))
(test/exn (parse '"Hello") "Expression should be number or AE")
(test/exn (parse '(mul 1 2)) "Only plus,minus or number in proper format is allowed")
(test/exn (parse '(minus 1 2 3)) "Expression not in proper format")
(test/exn (parse '(plus 1 2 3)) "Expression not in proper format")
(test/exn (parse '(3))  "Only plus,minus or number in proper format is allowed")
(test (parse '(minus 4 3)) (sub (num 4) (num 3)))
(test (parse '(plus (plus 1 2) (minus 3 4))) (add (add (num 1) (num 2)) (sub (num 3) (num 4))))


; ; ( unparse an−ae ) −> sexp
; ; an−ae : AE
( define unparse
( lambda ( an-ae )

(type-case AE an-ae
[ num(n) n]
[ add (l r) (cons 'plus (cons (unparse l) (cons (unparse r) empty)))]
[ sub (l r) (cons 'minus (cons (unparse l) (cons (unparse r) empty)))])))
 
(test (equal? ( parse '3) ( parse ( unparse ( parse '3)))) #t)
(test (equal? ( parse '(plus (plus 1 2) (minus 3 4))) ( parse ( unparse ( parse '(plus (plus 1 2) (minus 3 4)))))) #t)

(define (calc an-ae)
(type-case AE an-ae
[num (n) n]
[add (l r) (+ (calc l) (calc r))]
[sub (l r) (- (calc l) (calc r))]))

(test (calc (parse '3)) 3)
(test (calc (parse '(plus 1 2))) 3)
(test (calc (parse '(plus (plus 1 2) (minus 3 4)))) 2)
(test/exn (calc (parse '(3)))  "Only plus,minus or number in proper format is allowed")
(test/exn (calc (parse '"Hello")) "Expression should be number or AE")
(test/exn (calc (parse '(mul 1 2))) "Only plus,minus or number in proper format is allowed")
(test/exn (calc (parse '(minus 1 2 3))) "Expression not in proper format")
(test/exn (calc (parse '(plus 1 2 3))) "Expression not in proper format")


