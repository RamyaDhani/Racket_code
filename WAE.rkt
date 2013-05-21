#lang plai

(define-type WAE
[num (n number?)]
[add (lhs WAE?) (rhs WAE?)]
[sub (lhs WAE?) (rhs WAE?)]
[with (name symbol?) (named-expr WAE?) (body WAE?)]
[id (name symbol?)])

; ; ( parse an−ae ) −> WAE
; ; an−ae : sexp

(define (parse sexp)
  
  (cond
    [(number? sexp)(num sexp)]
    [(symbol? sexp) (id (car (cons sexp empty)))]
    [(list? sexp)
     (cond 
       [(> (length sexp) 3) 
        (error 'parse "Expression is to long: too many args")]
       [(< (length sexp) 3) 
        (error 'parse "Expression is to short: too few args")]
       [else
        (case (first sexp)
          [(plus)(add (parse (second sexp))
                      (parse (third sexp)))]
          [(minus)(sub (parse (second sexp))
                       (parse (third sexp)))]
          [(with) (if (list? (second sexp)) 
                         (if(= (length (second sexp)) 2) (with (car (cons (first (second sexp)) empty)) 
                                                               (parse (second (second sexp))) 
                                                               (parse (third sexp)))
                            (error "Expression in the substitution for id contains too few elements"))
                         
                         (error "Expression-with not in proper format"))]
           
          [else
           (error 'parse "Expected either plus or minus or with for first arg")])])]
    [else
     (error 'parse "Not a valid expression")]))

        
(test (parse '5) (num 5))
(test (parse '{plus 5 5}) (add (num 5) (num 5)))
(test (parse '{with {x {plus 5 5}} {plus x x}}) (with 'x (add (num 5)(num 5)) (add (id 'x) (id 'x))))
(test (parse '{with {x {plus 5 5}} {with {y {minus x 3}} {plus y y}}}) (with 'x (add (num 5) (num 5)) (with 'y (sub (id 'x) (num 3)) (add (id 'y) (id 'y)))))
(test/exn (parse '{with {x 5} {plus x x} {plus x x}}) "Expression is to long: too many args")
(test/exn (parse '{with {x 5}}) "Expression is to short: too few args")
(test/exn (parse '"Hello") "Not a valid expression")
(test/exn (parse '{with x {with {x x} x}}) "Expression-with not in proper format")
(test/exn (parse '{with (x) {with {x x} x}}) "Expression in the substitution for id contains too few elements")
(test/exn (parse '(with (x (plus y 5)) (with y (x x)))) "Expression-with not in proper format")


             

;; subst : WAE symbol WAE!WAE
(define (subst expr sub-id val)
(type-case WAE expr
[num (n) expr]
[add (l r) (add (subst l sub-id val)
(subst r sub-id val))]
[sub (l r) (sub (subst l sub-id val)
(subst r sub-id val))]
[with (bound-id named-expr bound-body)
(if (symbol=? bound-id sub-id)
(with bound-id
(subst named-expr sub-id val)
bound-body)
(with bound-id
(subst named-expr sub-id val)
(subst bound-body sub-id val)))]
[id (v) (if (symbol=? v sub-id) val expr)]))


;; calc : WAE!number
(define (calc expr)
(type-case WAE expr
[num (n) n]
[add (l r) (+ (calc l) (calc r))]
[sub (l r) (- (calc l) (calc r))]
[with (bound-id named-expr bound-body)
(calc (subst bound-body
bound-id
(num (calc named-expr))))]
[id (v) (error 'calc "free identifier")]))
                

(test (calc (parse '5)) 5)
(test (calc (parse '{plus 5 5})) 10)
(test (calc (parse '{with {x {plus 5 5}} {plus x x}})) 20)
(test (calc (parse '{with {x 5} {plus x x}})) 10)
(test (calc (parse '{with {x {plus 5 5}} {with {y {minus x 3}} {plus y y}}})) 14)
(test (calc (parse '{with {x 5} {with {y {minus x 3}} {plus y y}}})) 4)
(test (calc (parse '{with {x 5} {plus x {with {x 3} 10}}})) 15)
(test (calc (parse '{with {x 5} {plus x {with {x 3} x}}})) 8)
(test (calc (parse '{with {x 5} {plus x {with {y 3} x}}})) 10)
(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc (parse '{with {x 5} {with {x x} x}})) 5)

(test/exn (calc (parse '{with {x 5} {plus x x} {plus x x}})) "Expression is to long: too many args")
(test/exn (calc (parse '{with {x 5}})) "Expression is to short: too few args")
(test/exn (calc (parse '"Hello")) "Not a valid expression")
(test/exn (calc (parse '{with x {with {x x} x}})) "Expression-with not in proper format")
(test/exn (calc (parse '{with (x) {with {x x} x}})) "Expression in the substitution for id contains too few elements")
(test/exn (calc (parse '(with (x (plus y 5)) (with y (x x))))) "Expression-with not in proper format")

;; (unparse an-ae)-> sexp?
;; an-ae : WAE?
(define unparse
  (lambda (an-ae)
    (type-case WAE an-ae
      [num (n) n]
      [id (v) v]
      [add (l r) (list 'plus (unparse l) (unparse r))]
      [sub (l r) (list 'minus (unparse l) (unparse r))]
      [with (q l r) (list 'with (list q (unparse l)) (unparse r))] )))

(test (equal? (parse '5) (parse (unparse (parse '5)))) #t)
(test (equal? (parse '{plus 5 5}) (parse (unparse (parse '{plus 5 5})))) #t)
(test (equal? (parse '{with {x {plus 5 5}} {plus x x}}) (parse (unparse (parse '{with {x {plus 5 5}} {plus x x}})))) #t)
(test (equal? (parse '{with {x 5} {plus x x}}) (parse (unparse (parse '{with {x 5} {plus x x}})))) #t)
(test (equal? (parse '{with {x {plus 5 5}} {with {y {minus x 3}} {plus y y}}}) (parse (unparse (parse '{with {x {plus 5 5}} {with {y {minus x 3}} {plus y y}}})))) #t)
(test (equal? (parse '{with {x 5} {with {y {minus x 3}} {plus y y}}}) (parse (unparse (parse '{with {x 5} {with {y {minus x 3}} {plus y y}}})))) #t)
(test (equal? (parse '{with {x 5} {plus x {with {x 3} 10}}}) (parse (unparse (parse '{with {x 5} {plus x {with {x 3} 10}}})))) #t)
(test (equal? (parse '{with {x 5} {plus x {with {x 3} x}}}) (parse (unparse (parse '{with {x 5} {plus x {with {x 3} x}}})))) #t)
(test (equal? (parse '{with {x 5} {plus x {with {y 3} x}}}) (parse (unparse (parse '{with {x 5} {plus x {with {y 3} x}}})))) #t)
(test (equal? (parse '{with {x 5} {with {y x} y}}) (parse (unparse (parse '{with {x 5} {with {y x} y}})))) #t)
(test (equal? (parse '{with {x 5} {with {x x} x}}) (parse (unparse (parse '{with {x 5} {with {x x} x}})))) #t)




