#lang plai

(define-type FWAE::Binding
  [binding (id symbol?) (named-expr FWAE?)])

(define-type FWAE
  [FWAE::num (n number?)]
  [FWAE::add (lhs FWAE?) (rhs FWAE?)]
  [FWAE::div (lhs FWAE?) (rhs FWAE?)]
  [FWAE::id (name symbol?)]
  [FWAE::if0
   (cond-expr FWAE?)
   (zero-expr FWAE?)
   (non-zero-expr FWAE?)]
  [FWAE::with
   (bindings (listof FWAE::Binding?))
   (body FWAE?)]
  [FWAE::fun
   (params (listof symbol?))
   (body FWAE?)]
  [FWAE::app
   (fun-expr FWAE?)
   (arg-exprs (listof FWAE?))])



;; (parse sexp) -> FWAE?
;; sexp : s-exp?
(define FWAE::parse
  (lambda (sexp)
    (cond
      [(number? sexp)(FWAE::num sexp)]
      [(symbol? sexp) (FWAE::id sexp)]
      [(list? sexp)
       
       (cond
         
         [(> (length sexp) 4) 
          (error 'parse "Expression is to long: too many args")]
         [(< (length sexp) 2) 
          (error 'parse "Expression is to short: too few args")]
         
         [else
          (case (first sexp)
            [(plus)(if (= (length sexp) 3)
                       (FWAE::add (FWAE::parse (second sexp))
                                  (FWAE::parse (third sexp)))
                       (error "Error parsing add-expression - wrong number of elements"))]
            
            [(div) (if (= (length sexp) 3)
                       (FWAE::div (FWAE::parse (second sexp))
                                  (FWAE::parse (third sexp)))
                       (error "Error parsing div-expression - wrong number of elements"))]
            
            [(with) (if (and (list? (second sexp)) (= (length sexp) 3))
                        (if (empty? (second sexp))
                            (FWAE::with empty (FWAE::parse (third sexp)))
                            
                            (if (list? (first (second sexp)))
                                (FWAE::with 
                                 (map (lambda (with-expr)
                                        (if (= (length with-expr) 2)
                                            (if (and (symbol? (first with-expr)) 
                                                     (not (empty? with-expr)))
                                                (binding (first with-expr)
                                                         (FWAE::parse (second with-expr))) 
                                                (error "Error parsing with-binding"))
                                            (error "Error parsing with-binding")))
                                      (second sexp))
                                 (FWAE::parse (third sexp)))
                                (error "Error parsing with-binding")))
                        (error "Error parsing with-expression"))]
            
            
            [(if0) (if (= (length sexp) 4)
                       (FWAE::if0 
                        (if (empty? (second sexp))
                            (error "Error parsing if0- No condition")
                            (FWAE::parse (second sexp))) 
                        (FWAE::parse (third sexp)) 
                        (FWAE::parse (last sexp)))
                       (error "Error parsing if0-expression"))]
            
            [(fun) (if (list? (second sexp))
                       (if (= (length sexp) 3)
                           (FWAE::fun
                            (if (empty? (second sexp))
                                empty
                                (map (lambda (fun-expr) 
                                       (if (symbol? fun-expr)
                                           fun-expr
                                           (error "Error parsing function parameters")))
                                     (second sexp)))
                            (FWAE::parse (third sexp)))
                           (error "Error parsing fun expression - wrong number of elements"))
                       (error "Error parsing fun expression"))]
            
            
            [else (if (and (= (length sexp) 2) (equal? (first (first sexp)) 'fun))
                      
                      (FWAE::app 
                       (FWAE::parse (first sexp))
                       
                       (if (list? (second sexp))
                           (if (empty? (second sexp))
                               empty
                               
                               (map (lambda (app-sub-expr)
                                      (if (not (empty? app-sub-expr))
                                          (FWAE::parse app-sub-expr)
                                          (error "Error parsing app-expression-list second exp")))
                                    (second sexp)))
                           
                           (error "Error parsing app-expression")))
                      (error "Unexpected Expression"))])     
          ])]
      [else (error "Unexpected Expression")])
    ))



(test
 (FWAE::parse '{plus 1 2})
 (FWAE::add (FWAE::num 1) (FWAE::num 2)))

(test 
 (FWAE::parse '5) (FWAE::num 5))

(test
 (FWAE::parse 'x) (FWAE::id 'x))

(test 
 (FWAE::parse '{plus 5 5}) 
 (FWAE::add (FWAE::num 5) (FWAE::num 5)))

(test 
 (FWAE::parse '(with ((x 1)(y 2)) 
                     (plus x y)))
 (FWAE::with (list (binding 'x (FWAE::num 1)) 
                   (binding 'y (FWAE::num 2))) 
             (FWAE::add (FWAE::id 'x) (FWAE::id 'y))))


(test 
 (FWAE::parse '(with () (plus x y)))
 (FWAE::with '() (FWAE::add (FWAE::id 'x) 
                            (FWAE::id 'y))))

(test
 (FWAE::parse '(fun () (plus 2 2)))
 (FWAE::fun '() (FWAE::add (FWAE::num 2) 
                           (FWAE::num 2))))

(test 
 (FWAE::parse '(fun (x y) (plus x 4)))
 (FWAE::fun '(x y) (FWAE::add (FWAE::id 'x) 
                              (FWAE::num 4))))

(test
 (FWAE::parse '((fun (x) (plus x 4)) (5)))
 (FWAE::app (FWAE::fun '(x) (FWAE::add (FWAE::id 'x) 
                                       (FWAE::num 4)))
            (list (FWAE::num 5))))

(test
 (FWAE::parse '((fun (x y) (plus x y)) (5 10)))
 (FWAE::app (FWAE::fun '(x y) (FWAE::add (FWAE::id 'x) 
                                         (FWAE::id 'y))) 
            (list (FWAE::num 5) (FWAE::num 10))))

(test
 (FWAE::parse '(div 5 4))
 (FWAE::div (FWAE::num 5) 
            (FWAE::num 4)))

(test
 (FWAE::parse '(div (plus 5 6) 
                    (plus 4 4)))
 (FWAE::div (FWAE::add (FWAE::num 5) 
                       (FWAE::num 6)) 
            (FWAE::add (FWAE::num 4) 
                       (FWAE::num 4))))

(test
 (FWAE::parse '(if0 (plus 1 0) 
                  (plus 2 2)
                  (plus 3 3)))
(FWAE::if0 (FWAE::add (FWAE::num 1)
                    (FWAE::num 0))
          (FWAE::add (FWAE::num 2)
                    (FWAE::num 2))
          (FWAE::add (FWAE::num 3)
                    (FWAE::num 3))))


(test/exn 
 (FWAE::parse '(plus 1 2 3))
 "Error parsing add-expression - wrong number of elements")

(test/exn
 (FWAE::parse '(div 1 2 3))
 "Error parsing div-expression - wrong number of elements")
 
(test/exn
 (FWAE::parse '"Hello")
 "Unexpected Expression")

(test/exn
 (FWAE::parse '(minus 1 2))
  "Unexpected Expression")

(test/exn
 (FWAE::parse '(with x 5))
 "Error parsing with-expression")

(test/exn 
 (FWAE::parse '(with x))
 "Error parsing with-expression")

(test/exn
 (FWAE::parse '(with (x 1) 5))
 "Error parsing with-binding")

(test/exn
 (FWAE::parse '(with ((x 1) () (y 3)) 5))
 "Error parsing with-binding")

(test/exn
 (FWAE::parse '(if0 () (plus 1 2) (plus 2 3)))
 "Error parsing if0- No condition")

(test/exn
 (FWAE::parse '(if0 (= (plus 1 2) (plus 2 3)) 
                    (plus 5 6)
                    (div 6 5)
                    (div 7 5)))
 "Expression is to long: too many args")
 
(test/exn
 (FWAE::parse '(if0 (= (plus 1 2) (plus 2 3)) 
                    (plus 5 6)))
 "Error parsing if0-expression")

(test/exn
 (FWAE::parse '(fun (1) (plus 1 2)))
 "Error parsing function parameters")

(test/exn
 (FWAE::parse '(fun 1 (plus 1 2)))
 "Error parsing fun expression")

(test/exn
 (FWAE::parse '(fun (1) (plus 2 3) (plus 2 3)))
 "Error parsing fun expression - wrong number of elements")

(test/exn
 (FWAE::parse '((plus 1 2) (plus 2 3)))
  "Unexpected Expression")

(test/exn
 (FWAE::parse '((fun (x) (plus 1 2)) (1 ())))
 "Error parsing app-expression-list second exp")

(test/exn
 (FWAE::parse '((fun (x) (plus 1 2)) 1))
 "Error parsing app-expression")
              


;; (FWAE::unparse a-fwae) -> s-exp?
;; a-fwae : FWAE?
(define FWAE::unparse
  (lambda (a-fwae)
    (type-case FWAE a-fwae
      [FWAE::num (n) n]
      [FWAE::add (l r) (list 'plus (FWAE::unparse l) 
                             (FWAE::unparse r))]
      [FWAE::div (l r) (list 'div (FWAE::unparse l) 
                             (FWAE::unparse r))]
      [FWAE::id (v) v]
      [FWAE::if0 (q l r) (list 'if0 (FWAE::unparse q) 
                               (FWAE::unparse l) 
                               (FWAE::unparse r))]
      [FWAE::with (l r) (list 'with 
                              (if (empty? l) l
                                  (map (lambda (binding-expr)
                                         (cons (binding-id binding-expr) 
                                               (cons (FWAE::unparse 
                                                      (binding-named-expr binding-expr)) 
                                                     empty))) 
                                       l))
                              (FWAE::unparse r))]
      
      
      
      [FWAE::fun (l r) (list 'fun l                                       
                             (FWAE::unparse r))]
      
      [FWAE::app (l r) (list (FWAE::unparse l) 
                             (if (empty? r) r
                                 (map (lambda (arg-expr)
                                        (FWAE::unparse arg-expr))
                                      r)))])))






(test
 (FWAE::unparse (FWAE::num 0))
 '0)

(test
 (FWAE::unparse (FWAE::add (FWAE::num 1) 
                           (FWAE::num 2)))
 '{plus 1 2})
 

(test 
 (FWAE::unparse (FWAE::num 5))
 '5) 

(test
 (FWAE::unparse (FWAE::id 'x))
 'x) 

(test 
 (FWAE::unparse (FWAE::add (FWAE::num 5) 
                           (FWAE::num 5)))
 '{plus 5 5}) 


(test 
 (FWAE::unparse (FWAE::with (list (binding 'x (FWAE::num 1)) 
                                  (binding 'y (FWAE::num 2))) 
                            (FWAE::add (FWAE::id 'x) (FWAE::id 'y))))
 '(with ((x 1)(y 2)) 
        (plus x y)))
 


(test 
 (FWAE::unparse  (FWAE::with '() 
                             (FWAE::add (FWAE::id 'x) 
                                        (FWAE::id 'y))))
 
 '(with () (plus x y)))

(test
 (FWAE::unparse  (FWAE::fun '() (FWAE::add (FWAE::num 2) 
                           (FWAE::num 2))))
  '(fun () (plus 2 2)))


(test 
 (FWAE::unparse (FWAE::fun '(x y) (FWAE::add (FWAE::id 'x) 
                                             (FWAE::num 4))))
 '(fun (x y) (plus x 4)))


(test
 (FWAE::unparse (FWAE::app 
                 (FWAE::fun '(x) 
                            (FWAE::add (FWAE::id 'x) 
                                       (FWAE::num 4)))
                 (list (FWAE::num 5))))
 '((fun (x) (plus x 4)) (5)))


(test
 (FWAE::unparse (FWAE::app (FWAE::fun '(x y) 
                                      (FWAE::add (FWAE::id 'x) 
                                                 (FWAE::id 'y))) 
                           (list (FWAE::num 5) 
                                 (FWAE::num 10))))
 '((fun (x y) (plus x y)) (5 10)))
 

(test
 (FWAE::unparse (FWAE::div (FWAE::num 5) 
                           (FWAE::num 4)))
 '(div 5 4))


(test
 (FWAE::unparse  (FWAE::div (FWAE::add (FWAE::num 5) 
                                       (FWAE::num 6)) 
                            (FWAE::add (FWAE::num 4) 
                                       (FWAE::num 4))))
 '(div (plus 5 6) 
       (plus 4 4)))



;; (FWAE::duplicate-params? a-fwae) -> boolean?
;; a-fae : FAE?
(define FWAE::duplicate-params?
  (lambda (a-fae)
    (type-case FWAE a-fae
      [FWAE::fun (l r)  (or (if (empty? l) #f
                                (if (<
                                     (length (set->list (apply set l)))
                                     (length l))
                                    #t
                                    #f))
                           (FWAE::duplicate-params? r))]
      [FWAE::num (n) #f]
      
      [FWAE::id (v) #f]
      
      [FWAE::add (l r) (or  (FWAE::duplicate-params? l) 
                             (FWAE::duplicate-params? r))]
      
      [FWAE::div (l r) (or  (FWAE::duplicate-params? l) 
                             (FWAE::duplicate-params? r))]
      
      [FWAE::if0 (q l r) (or (FWAE::duplicate-params? q) 
                              (FWAE::duplicate-params? l) 
                              (FWAE::duplicate-params? r))]
      
      [FWAE::with (l r) (or  (if (empty? l) #f
                                  (andmap (lambda (binding-expr)
                                          (FWAE::duplicate-params? (binding-named-expr binding-expr)))         
                                       l))
                              (FWAE::duplicate-params? r))]
      [FWAE::app (l r) (or (FWAE::duplicate-params? l) 
                             (if (empty? r) #f
                                 (andmap (lambda (arg-expr)
                                        (FWAE::duplicate-params? arg-expr))
                                      r)))])))



(test
 (FWAE::duplicate-params? (FWAE::parse '{fun {} 2}))
 #f)

(test
 (FWAE::duplicate-params? (FWAE::parse '((fun (x x) 
                                              (plus x y)) 
                                         (5 10))))
 #t)

(test 
 (equal? (FWAE::parse '5) 
         (FWAE::parse (FWAE::unparse (FWAE::parse '5)))) 
 #t)

(test (equal? (FWAE::parse '{plus 5 5}) 
              (FWAE::parse (FWAE::unparse (FWAE::parse '{plus 5 5})))) 
#t)

