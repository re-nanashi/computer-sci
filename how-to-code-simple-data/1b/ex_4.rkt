(require 2htdp/image)

(define IMAGE1 (rectangle 10 15 "solid" "red"))
(define IMAGE2 (rectangle 15 10 "solid" "red"))
(define IMAGE3 (square 15 "solid" "red"))

;; Image -> Boolean
;; produce true if img is tall (height is > width)
(check-expect (tall? IMAGE1) true)
(check-expect (tall? IMAGE2) false)
(check-expect (tall? IMAGE3) false)

;(define (tall? img) false)     ;stub

;(define (tall? img)            ;template
;  (... img))

(define (tall? img)
  (> (image-height img) (image-weight img)))
