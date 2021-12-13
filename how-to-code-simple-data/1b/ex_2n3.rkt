(require 2htdp/image)

(define IMAGE1 (rectangle 10 15 "solid" "red"))
(define IMAGE2 (rectangle 15 10 "solid" "red"))

;; Image -> area
;; produces an area given an image
(check-expect (areaFromImage IMAGE1) "150")
(check-expect (areaFromImage IMAGE2) "150")

;(define (areaFromImage image) "0") ;stub

;(define (areaFromImage image)      ;template
;  ... image)

(define (areaFromImage image)
  (* (image-width image) 
     (image-height image)))
