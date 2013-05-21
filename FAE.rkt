#lang plai

(define-type FAE
  [FAE::num (n number?)]
  [FAE::add (lhs FAE?) (rhs FAE?)]
  [FAE::div (lhs FAE?) (rhs FAE?)]
  [FAE::id (name symbol?)]
  [FAE::if0
   (cond-expr FAE?)
   (zero-expr FAE?)
   (non-zero-expr FAE?)]
  [FAE::fun
   (param symbol?)
   (body FAE?)]
  [FAE::app
   (fun-expr FAE?)
   (arg-expr FAE?)])

;; (FAE::parse sexp) -> FAE?
;; sexp : list?
(define FAE::parse
  (lambda (sexp)
    (cond
      [(number? sexp)(FAE::num sexp)]
      [(symbol? sexp) (FAE::id sexp)]
      [(list? sexp)
       (cond
         
         [(> (length sexp) 4) 
          (error 'parse "Expression is to long: too many args")]
         [(< (length sexp) 2) 
          (error 'parse "Expression is to short: too few args")]
         [else
          (case (first sexp)
            [(plus)(if (= (length sexp) 3)
                       (FAE::add (FAE::parse (second sexp))
                                 (FAE::parse (third sexp)))
                       (error "Error parsing add-expression - wrong number of elements"))]
            
            
            [(div) (if (= (length sexp) 3)
                       (FAE::div (FAE::parse (second sexp))
                                 (FAE::parse (third sexp)))
                       (error "Error parsing div-expression - wrong number of elements"))]
            
            [(if0) (if (= (length sexp) 4)
                       (FAE::if0 
                        (if (empty? (second sexp))
                            (error "Error parsing if0- No condition")
                            (FAE::parse (second sexp))) 
                        (FAE::parse (third sexp)) 
                        (FAE::parse (last sexp)))
                       (error "Error parsing if0-expression"))]
            
            [(fun)  (if (= (length sexp) 3)
                        (if (symbol? (second sexp))
                            (FAE::fun (second sexp) 
                                      (FAE::parse (third sexp)))
                            (error "Error parsing function parameters"))
                        (error "Error parsing fun expression"))]
            [else (if (and (= (length sexp) 2) (equal? (first (first sexp)) 'fun))
                      (FAE::app 
                       (FAE::parse (first sexp))
                       (FAE::parse (second sexp)))
                      (error "Error parsing app-expression"))])])]
      
      [else (error "Unexpected Expression")])))



(test 
 (FAE::parse '5) (FAE::num 5))

(test
 (FAE::parse 'x) (FAE::id 'x))

(test 
 (FAE::parse '{plus 5 5}) 
 (FAE::add (FAE::num 5) (FAE::num 5)))


(test
 (FAE::parse '(fun x (plus 2 2)))
 (FAE::fun 'x (FAE::add (FAE::num 2) 
                        (FAE::num 2))))

(test
 (FAE::parse '(div 5 4))
 (FAE::div (FAE::num 5) 
           (FAE::num 4)))

(test
 (FAE::parse '(div (plus 5 6) 
                   (plus 4 4)))
 (FAE::div (FAE::add (FAE::num 5) 
                     (FAE::num 6)) 
           (FAE::add (FAE::num 4) 
                     (FAE::num 4))))

(test
 (FAE::parse '(if0 (plus 1 0) 
                   (plus 2 2)
                   (plus 3 3)))
 (FAE::if0 (FAE::add (FAE::num 1)
                     (FAE::num 0))
           (FAE::add (FAE::num 2)
                     (FAE::num 2))
           (FAE::add (FAE::num 3)
                     (FAE::num 3))))

(test 
 (FAE::parse '((fun x (plus x 1))
               (plus 2 2)))
 
 (FAE::app (FAE::fun 'x (FAE::add (FAE::id 'x)
                                  (FAE::num 1)))
           (FAE::add (FAE::num 2)
                     (FAE::num 2))))

(test/exn
 (FAE::parse '((plus 1 2) (plus 2 3)))
 "Error parsing app-expression")


(test/exn 
 (FAE::parse '(plus 1 2 3))
 "Error parsing add-expression - wrong number of elements")

(test/exn
 (FAE::parse '(div 1 2 3))
 "Error parsing div-expression - wrong number of elements")

(test/exn
 (FAE::parse '"Hello")
 "Unexpected Expression")

(test/exn
 (FAE::parse '(if0 () (plus 1 2) (plus 2 3)))
 "Error parsing if0- No condition")

(test/exn
 (FAE::parse '(if0 (= (plus 1 2) (plus 2 3)) 
                   (plus 5 6)
                   (div 6 5)
                   (div 7 5)))
 "Expression is to long: too many args")

(test/exn
 (FAE::parse '(if0 (= (plus 1 2) (plus 2 3)) 
                   (plus 5 6)))
 "Error parsing if0-expression")

(test/exn
 (FAE::parse '(fun 1 (plus 1 2)))
 "Error parsing function parameters")

(test/exn
 (FAE::parse '(fun x (plus 2 3) (plus 2 3)))
 "Error parsing fun expression")

(test/exn
 (FAE::parse '((fun x (plus 1 2))))
 "Expression is to short: too few args")




;; (FAE::unparse a-fae) -> list?
;; a-fwae : FAE?
(define FAE::unparse
  (lambda (a-fae)
    (type-case FAE a-fae
      [FAE::num (n) n]
      [FAE::add (l r) (list 'plus (FAE::unparse l) 
                            (FAE::unparse r))]
      [FAE::div (l r) (list 'div (FAE::unparse l) 
                            (FAE::unparse r))]
      [FAE::id (v) v]
      [FAE::if0 (q l r) (list 'if0 (FAE::unparse q) 
                              (FAE::unparse l) 
                              (FAE::unparse r))]
      [FAE::fun (l r) (list 'fun l                                       
                            (FAE::unparse r))]
      
      [FAE::app (l r) (list (FAE::unparse l) 
                            (FAE::unparse r))])
    
    ))

(test 
 (FAE::unparse (FAE::num 5)) 5)

(test
 (FAE::unparse (FAE::id 'x)) 'x)

(test 
 (FAE::unparse (FAE::add (FAE::num 5) 
                         (FAE::num 5)))
 '{plus 5 5})


(test
 (FAE::unparse (FAE::fun 'x 
                         (FAE::add (FAE::num 2) 
                                   (FAE::num 2))))
 '(fun x (plus 2 2)))


(test
 (FAE::unparse (FAE::div (FAE::num 5) 
                         (FAE::num 4)))
 '(div 5 4))

(test
 (FAE::unparse  (FAE::div (FAE::add (FAE::num 5) 
                                    (FAE::num 6)) 
                          (FAE::add (FAE::num 4) 
                                    (FAE::num 4))))
 '(div (plus 5 6) 
       (plus 4 4)))


(test
 (FAE::unparse (FAE::if0 (FAE::add (FAE::num 1)
                                   (FAE::num 0))
                         (FAE::add (FAE::num 2)
                                   (FAE::num 2))
                         (FAE::add (FAE::num 3)
                                   (FAE::num 3))))
 
 '(if0 (plus 1 0) 
       (plus 2 2)
       (plus 3 3)))

(test 
 (FAE::unparse  (FAE::app (FAE::fun 'x (FAE::add (FAE::id 'x)
                                                 (FAE::num 1)))
                          (FAE::add (FAE::num 2)
                                    (FAE::num 2))))
 
 '((fun x (plus x 1))
   (plus 2 2)))




;; subst : FAE symbol FAE -> FAE
(define (subst expr sub-id val)
  
  (type-case FAE expr
    [FAE::num (n) expr]
    [FAE::add (l r) (FAE::add (subst l sub-id val)
                              (subst r sub-id val))]
    [FAE::div (l r) (FAE::div (subst l sub-id val)
                              (subst r sub-id val))]
    [FAE::if0 (q l r) (FAE::if0 (subst q sub-id val)
                                (subst l sub-id val
                                       (subst r sub-id val)))]
    [FAE::id (v) (if (symbol=? v sub-id) val expr)]
    [FAE::fun (bound-id bound-body)
              (if (symbol=? bound-id sub-id)
                  expr            
                  (subst bound-body sub-id val))]
    [FAE::app (fun-expr arg-expr)
              (FAE::app (subst fun-expr sub-id val)
                        (subst arg-expr sub-id val))])
  )

;; subst1 : FAE symbol FAE -> FAE
(define (subst1 expr sub-id val)
  
  (type-case FAE expr
    [FAE::num (n) expr]
    [FAE::add (l r) (FAE::add (subst1 l sub-id val)
                              (subst1 r sub-id val))]
    [FAE::div (l r) (FAE::div (subst1 l sub-id val)
                              (subst1 r sub-id val))]
    [FAE::if0 (q l r) (FAE::if0 (subst1 q sub-id val)
                                (subst1 l sub-id val
                                        (subst1 r sub-id val)))]
    [FAE::id (v) (if (symbol=? v sub-id) val expr)]
    [FAE::fun (bound-id bound-body)
              (if (symbol=? bound-id sub-id)
                  expr            
                  (subst1 bound-body sub-id val))]
    [FAE::app (fun-expr arg-expr)
              (FAE::app (subst1 fun-expr sub-id arg-expr)
                        arg-expr)])
  )
;; (FAE::interp a-fae order) -> FAE?
;; a-fae : FAE?
;; order : symbol?
(define FAE::interp
  (lambda (a-fae order)
    (if (symbol=? order 'c-b-v)
        (letrec ([my-interp
                  (lambda (a-fae)
                    (type-case FAE a-fae
                      [FAE::num (n) a-fae] 
                      [FAE::add (l r) (if (and (FAE::num? (my-interp l)) (FAE::num? (my-interp r)))
                                          (FAE::num (+ (FAE::num-n  (my-interp l))(FAE::num-n  (my-interp r))))
                                          (error "Argument to arithmetic operation not a number"))]
                                           
                      [FAE::div (l r) (if (and (FAE::num? (my-interp l)) (FAE::num? (my-interp r)))
                                (FAE::num (/ (FAE::num-n  (my-interp l))(FAE::num-n  (my-interp r))))
                                (error "Argument to arithmetic operation not a number"))]
                      [FAE::id (v)(error "Free identifier")]
                      [FAE::if0 (q l r) (if (my-interp q)
                                            (my-interp l)
                                            (my-interp r))]
                      [FAE::fun (bound-id bound-body) (FAE::fun bound-id bound-body)]
                      [FAE::app (fun-expr arg-expr)
                                (if (FAE::fun? fun-expr)
                                (let ((fun-val (my-interp fun-expr))
                                      (arg-val (my-interp arg-expr)))
                                  (my-interp (subst (FAE::fun-body fun-val) (FAE::fun-param fun-val) arg-val)))
                                (error "Left-hand side of application not a function"))
                                ]))])
          (my-interp a-fae))
        
        (letrec ([my-interp
                  (lambda (a-fae)
                    (type-case FAE a-fae
                      [FAE::num (n) a-fae] 
                      [FAE::add (l r) (if (and (FAE::num? (my-interp l)) (FAE::num? (my-interp r)))
                                          (FAE::num (+ (FAE::num-n  (my-interp l))(FAE::num-n  (my-interp r))))
                                          (error "Argument to arithmetic operation not a number"))]
                                           
                      [FAE::div (l r) (if (and (FAE::num? (my-interp l)) (FAE::num? (my-interp r)))
                                (FAE::num (/ (FAE::num-n  (my-interp l))(FAE::num-n  (my-interp r))))
                                (error "Argument to arithmetic operation not a number"))]

                      [FAE::id (v)(error "Free identifier")]
                      [FAE::if0 (q l r) (if (= (FAE::num-n (my-interp q)) 0)
                                            (my-interp r)
                                            (my-interp l))]
                      [FAE::fun (bound-id bound-body) (FAE::fun bound-id (my-interp bound-body))]
                      [FAE::app (fun-expr arg-expr)  
                                (if (FAE::fun? fun-expr)
                                (my-interp (subst1 (FAE::fun-body fun-expr) (FAE::fun-param fun-expr) arg-expr))
                                (error "Left-hand side of application not a function"))]))])
          (my-interp a-fae)))
    
    
    ))

(test/exn
 (FAE::interp (FAE::app (FAE::fun 'x (FAE::num 2)) (FAE::id 'y)) 'c-b-v)
 "Free identifier")

(test
 (FAE::interp (FAE::app (FAE::fun 'x (FAE::num 2)) (FAE::id 'y)) 'normal)
 (FAE::num 2))

(test
 (FAE::interp (FAE::app (FAE::fun 'x (FAE::add (FAE::num 2) (FAE::num 5))) (FAE::id 'y)) 'normal)
 (FAE::num 7))

(test
 (FAE::interp (FAE::if0 (FAE::add (FAE::num 0) (FAE::num 0))                           
                        (FAE::num 1)
                        (FAE::num 2)) 'normal)
 (FAE::num 2))

(test
 (FAE::interp (FAE::if0 (FAE::add (FAE::num 2) (FAE::num 5))                           
                        (FAE::num 1)
                        (FAE::num 0)) 'c-b-v)
 (FAE::num 1))

(test/exn
 (FAE::interp (FAE::app (FAE::num 5) (FAE::id 'y)) 'normal)
 "Left-hand side of application not a function")

(test/exn
 (FAE::interp (FAE::app (FAE::num 5) (FAE::id 'y)) 'c-b-v)
 "Left-hand side of application not a function")

(test/exn
 (FAE::interp (FAE::add (FAE::fun 'x (FAE::num 5)) (FAE::num 5)) 'c-b-v)
 "Argument to arithmetic operation not a number")
 
(test
 (FAE::interp (FAE::div (FAE::num 5) (FAE::num 5)) 'c-b-v)
 (FAE::num 1))

(test
 (FAE::interp (FAE::div (FAE::num 5) (FAE::num 5)) 'normal)
 (FAE::num 1))

;; (FAE::interpreter a-fae order) -> number?
;; a-fae : FAE?
;; order : symbol?
(define FAE::interpreter
  (lambda (a-fae order)
    (let ([res (FAE::interp a-fae order)])
      (if (FAE::num? res)
          (FAE::num-n res)
          (error 'interpreter "Result value not a number.")))))

(test/exn (FAE::interpreter (FAE::fun 'x (FAE::id 'x)) 'c-b-v)
          "Result value not a number.")
