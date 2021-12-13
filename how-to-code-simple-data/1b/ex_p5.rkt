(require 2htdp/image)

(define solidBlue40InchTriangle (triangle 40 "solid" "blue"))
(define solidBlue30InchTriangle (triangle 30 "solid" "blue"))
;; Number -> Image
;; produces a solid blue triangle given length of the sides
(check-expect (createBlueTriangle 40) solidBlue40InchTriangle)
(check-expect (createBlueTriangle 30) solidBlue30InchTriangle)

;;(define (createBlueTriangle len)       ;;stub
;;  (triangle 30 "solid" "blue"))

;;(define (createBlueTriangle len)       ;;template
;;  (...len))

(define (createBlueTriangle len)
  (triangle len "solid" "blue"))
