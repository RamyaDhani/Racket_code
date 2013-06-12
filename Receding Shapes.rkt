#lang racket
(require picturing-programs)



; ; ( make−frame w) −> image ?
; ; w : image ?
(define make-frame
( lambda (w)
( overlay w
( square 400 "solid" "white" ) ) ) )

; ; ( decrease-image w) −> image ?
; ; w : image ?
(define decrease-image
( lambda(w)
  (cond [(> (image-width w) 0) (scale (/ 127 128) w)]
        [else w])))





; ; ( new−image w x y mouse−event ) −> image?
; ; w : image?
; ; x : integer?
; ; y : integer?
; ; mouse−event : mouse−event?
(define new-image
(lambda (w x y mouse-event)
   (cond [(string=? mouse-event "button-up")
  (case (random 5) 
       [(0) (square 400  "solid" (list-ref (list "red" "black" "blue" "yellow" "green") (random 5)))]
       [(1)   (circle 200 "solid" (list-ref (list "red" "black" "blue" "yellow" "green") (random 5)))]
       [(2)   (rhombus 400 45 "solid" (list-ref (list "red" "black" "blue" "yellow" "green") (random 5)))]
       [(3)  (triangle 400 "solid" (list-ref (list "red" "black" "blue" "yellow" "green") (random 5)))]
       [(4)  (star 400 "solid"(list-ref (list "red" "black" "blue" "yellow" "green") (random 5)))] )]      
       [else w])))
  

; ; (run−animation show−state) −> image?
; ; show−state : boolean?
(define run-animation
(lambda (show-state)
(big-bang empty-image
(state show-state)
(on-mouse new-image)
(on-tick decrease-image)
(to-draw make-frame))))
