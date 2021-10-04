(require 2htdp/image)

;; Color -> Image
;; produce a 10x10 square of a given color
(check-expect (createSquare "red") (square 10 "solid" "red"))
(check-expect (createSquare "blue") (square 10 "solid" "blue"))
(check-expect (createSquare "yellow") (square 10 "solid" "yellow"))

;; (define (createSquare color) 
;;      (square 10 "solid" "red"))  ;stub

;; (define (createSquare color)
;;      (... color))                ;template

(define (createSquare color)
  (square 10 "solid" color))
