#lang racket/base
(require 2htdp/image)

(define (createSquare len color) (rectangle len len "solid" color))

(define (createColorfulSquare len color1 color2)
    (above (beside (createSquare len color1)(createSquare len color2))
            (beside (createSquare len color2)(createSquare len color1))))

(createColorfulSquare 10 "blue" "yellow")
