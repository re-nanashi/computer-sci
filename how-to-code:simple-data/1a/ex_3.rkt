(require 2htdp/image)

(define (createSquare len color1 color2)
  (above (beside (square len "solid" color1) (square len "solid" color2))
         (beside (square len "solid" color2) (square len "solid" color1))))

(createSquare 20 "blue" "yellow")
