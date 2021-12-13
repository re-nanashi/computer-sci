(require 2htdp/image)

;; ==============
;; Constants:

(define BLANK (square 0 "outline" "white"))

;; ==============
;; Expressions:

(local [(define (gen_ellipse n)
          (ellipse n (* 2 n) "solid" "blue"))] 
  (build-list 20 gen_ellipse))

(local [(define (gen_ellipse n) 
          (ellipse n (* 2 n) "solid" "blue"))] 
  (foldr beside BLANK (build-list 20 gen_ellipse)))

(local [(define (gen_ellipse n) 
          (ellipse n (* 2 n) "solid" "blue"))] 
  (foldl beside BLANK (build-list 20 gen_ellipse)))

