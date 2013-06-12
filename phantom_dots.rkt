(require picturing-programs)
(provide phantom-dots)
(require rackunit)
(provide phantom-dots)

;;(phantom-dots background-color square-color l) -> image?
;; background-color : image-color?
;; square-color : image-color?
;; l : exact-nonnegative-integer?



(define phantom-dots 
  (lambda (background-color square-color l)
    (let* ([s (square (* 3 l) "solid" square-color)]
           [h1 (overlay/offset s (* 4 l) 0 s)]
           [h2 (overlay/offset h1 (* 6 l) 0 s)]
           [h3 (overlay/offset h2 (* 8 l) 0 s)]
           [h4 (overlay/offset h3 (* 10 l) 0 s)]
           [v1 (overlay/offset h4 0 (* 4 l) h4)]
           [v2 (overlay/offset v1 0 (* 6 l) h4)]
           [v3 (overlay/offset v2 0 (* 8 l) h4)]
           [v4 (overlay/offset v3 0 (* 10 l) h4)]
           [bs (square (* 19 l) "solid" background-color)]
           [finimage (overlay v4 bs)])         
      finimage)))

(check-expect (image-width (phantom-dots "red" "blue" 20)) 380)
(check-expect (image-height (phantom-dots "red" "blue" 20)) 380)
