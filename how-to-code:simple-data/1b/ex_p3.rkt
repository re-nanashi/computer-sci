(require 2htdp/image)

(define IMAGE1 (ellipse 60 30 "solid" "red"))
(define resultingImage (overlay (rectangle 60 30 "outline" "black") IMAGE1))

;; Image -> Image
;; produces an image that puts a box around the given image
(check-expect (boxify IMAGE1) resultingImage)

;;(define (boxify img) 0)       ;;stub

;;(define (boxify img)          ;;template
;;(...img))

(define (boxify img)
  (overlay 
    (rectangle (+ (image-width img) 2)
               (+ (image-height img) 2) 
               "outline" "black")
    img))
