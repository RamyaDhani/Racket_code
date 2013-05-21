#lang plai

(print-only-errors)

(define-type COMPARISON
  [GT]
  [EQ]
  [LT])


;; tree : 2-tree
;; cmp : (-> any/c any/c) -> COMPARISON?
(struct BST
  (tree cmp))

;; a simple binary tree data structure
(define-type 2-tree
  [empty-tree]
  [node
   (l 2-tree?)
   (v any/c)
   (r 2-tree?)])

;; (cmp-num x y) -> COMPARISON
;; x : num?
;; y : num?
(define cmp-num
  (lambda (x y)
    (cond
      [(> x y) (GT)]
      [(= x y) (EQ)]
      [(< x y) (LT)])))

;; (cmp-string x y) -> COMPARISON
;; x : num?
;; y : num?
(define cmp-string
  (lambda (x y) 
    (cond  
      [(string>? x y) (GT)]
      [(string=? x y) (EQ)]
      [(string<? x y) (LT)] )))




;; (BST-insert a-bst element) -> BST?
;; a-bst : BST?
;; element : any/c
(define BST-insert  
  (lambda (a-bst element)
    
    (if (BST? a-bst)
        
        (type-case 2-tree (BST-tree a-bst)
          [empty-tree ()
                      (BST 
                       (make-node (empty-tree) 
                                  element 
                                  (empty-tree))
                       (BST-cmp a-bst))]
          
          [node (l v r)
                (cond  
                  [(LT? ((BST-cmp a-bst) v element)) 
                   
                   (BST
                    (make-node l v
                               
                               (BST-tree         
                                (BST-insert 
                                 (BST
                                  r
                                  (BST-cmp a-bst))
                                 element))
                               )
                    (BST-cmp a-bst))
                   
                   ]
                  
                  [(GT? ((BST-cmp a-bst) v element))
                   
                   (BST
                    (make-node  (BST-tree (BST-insert 
                                           (BST l 
                                                (BST-cmp a-bst))
                                           element))
                                v r)
                    (BST-cmp a-bst))]
                  
                  [(EQ? ((BST-cmp a-bst) v element))
                   
                   (BST
                    (make-node  (BST-tree (BST-insert 
                                           (BST l 
                                                (BST-cmp a-bst))
                                           element))
                                v r)
                    (BST-cmp a-bst))])])
        
        (error "Not BST"))))

(test  
 (BST-insert (BST (empty-tree) 
                  cmp-num) 23)
 (BST (node (empty-tree) 
            23 (empty-tree))
      cmp-num))

(test
 (BST-insert
  (BST
   (node (empty-tree) 23 
         (empty-tree))
   cmp-num)
  24)
 (BST
  (node
   (empty-tree)
   23
   (node (empty-tree) 24 
         (empty-tree)))
  cmp-num))

(define my-bst 
  (BST (empty-tree) 
       cmp-num))
(define my-bst1 
  (BST-insert my-bst 
              23))
(define my-bst2 
  (BST-insert my-bst1
              22))
(define my-bst3 
  (BST-insert my-bst2
              24))
(define my-bst4 
  (BST-insert my-bst3 
              25))

(test my-bst2 
      (BST
       (node (node (empty-tree) 
                   22 
                   (empty-tree)) 
             23 
             (empty-tree))
       cmp-num))



(test my-bst3
      (BST
       (node (node (empty-tree) 
                   22 
                   (empty-tree)) 
             23 
             (node (empty-tree) 
                   24 
                   (empty-tree)))
       cmp-num))



(test my-bst4
      (BST
       (node (node (empty-tree) 
                   22 
                   (empty-tree))
             23 
             (node (empty-tree) 
                   24 
                   (node (empty-tree) 
                         25 
                         (empty-tree))))
       cmp-num))

(define my-bst5 (BST-insert my-bst4 20))  
(test my-bst5
      (BST
       (node (node (node (empty-tree) 
                         20 
                         (empty-tree)) 
                   22 
                   (empty-tree)) 
             23 
             (node (empty-tree) 
                   24 
                   (node (empty-tree) 
                         25 
                         (empty-tree))))
       cmp-num))

(test/exn (BST-insert (node (empty-tree)
                            20 
                            (empty-tree)) 
                      23)
          "Not BST")

(define s-bst (BST (empty-tree) cmp-string))
(define s-bst1 (BST-insert s-bst "a"))
(define s-bst2 (BST-insert s-bst1 "c"))
(define s-bst3 (BST-insert s-bst2 "b"))


(test s-bst1
      (BST
       (node (empty-tree) 
             "a" 
             (empty-tree))
       cmp-string))

(test s-bst2
      (BST
       (node (empty-tree)
             "a" 
             (node (empty-tree) 
                   "c" 
                   (empty-tree)))
       cmp-string))

(test s-bst3
      (BST
       (node (empty-tree) 
             "a" 
             (node (node (empty-tree) 
                         "b" 
                         (empty-tree)) 
                   "c" 
                   (empty-tree)))
       cmp-string))





;; A-BST is a tree into which have been inserted
;; 10 random natural numbers in the range [0,200), 200,
;; and then another 10 natural numbers in the same range
(define A-BST
  (foldr
   (lambda (x y) (BST-insert y x))
   (BST (empty-tree) cmp-num)
   (append
    (map random (make-list 10 200))
    '(200)
    (map random (make-list 10 200)))))

;; AN-BST is a tree into which have been inserted
;; 10 random natural numbers in the range [0,-200), -200,
;; and then another 10 natural numbers in the same range
(define AN-BST
  (foldr
   (lambda (x y) (BST-insert y x))
   (BST (empty-tree) cmp-num)
   (append
    (map (lambda (x) (- (random x))) (make-list 10 200))
    (cons -200 empty)
    (map (lambda (x) (- (random x))) (make-list 10 200)))))


;; (BST-max-element a-bst) -> any?
;; a-bst : BST?
(define BST-max-element
  (lambda (a-bst)
    (letrec ([max-e
              (lambda (a-tree)
                (type-case 2-tree a-tree
                  [empty-tree ()
                              (error 'max-element "Empty tree")]
                  [node (l v r)
                        (type-case 2-tree r
                          [empty-tree () v]
                          [else (max-e r)])]))])
      (max-e (BST-tree a-bst)))))



(test/exn
 (BST-max-element (BST (empty-tree) cmp-num))
 "Empty tree")

(test
 (BST-max-element A-BST)
 200)

(test
 (BST-max-element s-bst3)
 "c")

;; (BST-min-element a-bst) -> any/c
;; a-bst : BST?
(define BST-min-element
  (lambda (a-bst)
    (letrec ([min-e
              (lambda (a-tree)
                (type-case 2-tree a-tree
                  [empty-tree ()
                              (error 'min-element "Empty tree")]
                  [node (l v r)
                        (type-case 2-tree l
                          [empty-tree () v]
                          [else (min-e l)])]))])
      (min-e (BST-tree a-bst)))))

(test
 (BST-min-element AN-BST)
 -200)

(test/exn
 (BST-min-element (BST (empty-tree) cmp-num))
 "Empty tree")

(test
 (BST-min-element s-bst3)
 "a")


;; (BST->list a-bst) -> list
;; a-bst : BST?
(define BST->list
  (lambda (a-bst)
    (type-case 2-tree (BST-tree a-bst)
      [empty-tree () empty]
      [node (l v r)
            (append
             (if (not (empty-tree? l))
                 (BST->list (BST l cmp-num))
                 empty)
             (cons v empty)
             
             (if (not (empty-tree? r))
                 (BST->list (BST r cmp-num))
                 empty))
            
            ])))



(let ([a-list (range 32)])
  (let ([s-list (shuffle a-list)])
    (test
     (BST->list
      (foldr
       (lambda (x y) (BST-insert y x))
       (BST (empty-tree) cmp-num)
       s-list))
     a-list)))

;; (BST-member a-bst element) -> boolean?
(define BST-member
  (lambda (a-bst element)
    (or (type-case 2-tree (BST-tree a-bst)
          [empty-tree () #f]
          [node (l v r) 
                (or
                 (type-case 2-tree l
                   [empty-tree () #f]
                   [else (BST-member (BST l 
                                          (BST-cmp a-bst)) 
                                     element)])
                 (if (equal? v element)
                     #t
                     #f)
                 
                 (type-case 2-tree r
                   [empty-tree () #f]
                   [else (BST-member (BST r
                                          (BST-cmp a-bst))
                                     element)]))]))))



(test
 (BST-member A-BST 200)
 true)

(test
 (BST-member A-BST 201)
 false)



;; (BST-foldr f e a-bst) -> any/c
;; f : (-> any/c any/c any/c any/c)
;; e : any/c
;; a-bst : BST?
(define BST-foldr
  (lambda (f e a-bst)
    (type-case 2-tree (BST-tree a-bst)
      [empty-tree ()  e ]
      [node (l v r) 
            (BST-foldr f 
                       (f v (BST-foldr f 
                                       e 
                                       (BST r (BST-cmp a-bst)))) 
                       (BST l (BST-cmp a-bst)))])))


;; (BST-max-element-foldr a-bst) -> any/c
;; a-bst : BST?
(define BST-max-element-foldr
  (lambda (a-bst)
    (BST-foldr
     (lambda (e r1)
       (if (equal? (BST-cmp a-bst) 
                   cmp-num)
           (if (> e r1)
               e
               r1)
           (if (string>? e r1)
               e
               r1)))
     (node-v (BST-tree a-bst))
     a-bst)))

(test
 (BST-max-element-foldr A-BST)
 200)

(test
 (BST-max-element-foldr s-bst3)
 "c")

;; (BST-level-order a-bst) -> list?
;; a-bst : BST?
(define BST-level-order
  (lambda (a-bst)
    '()))


;; (BST-delete a-bst element) -> BST?
;; a-bst : BST?
;; element : any/c
(define BST-delete
  (lambda (a-bst element)
    a-bst))
