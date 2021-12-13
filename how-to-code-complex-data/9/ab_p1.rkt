(require 2htdp/image)

;; ListOfImages -> ListOfImages
;; produce a list containing only those images that are wider than they are tall
(check-expect (wide_images empty) empty)
(check-expect (wide_images (list (square 10 "solid" "white"))) empty)
(check-expect (wide_images (list (rectangle 10 14 "solid" "white") (rectangle 14 10 "solid" "white"))) 
              (list (rectangle 14 10 "solid" "white")))

;(define (wide_images loi) empty)    ; stub

(define (wide_images loi)
  (local [(define (wide? i) (> (image-width i) (image-height i)))]
   (filter wide? loi)))
