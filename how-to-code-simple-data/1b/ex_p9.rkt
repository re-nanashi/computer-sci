(require racket/math)

;; Number Number Number Number -> Number
;; produces the distance between the given two points (x1, y1), (x2, y2)
(check-expect (distance 3 0 0 4) 5)
(check-within (distance 1 0 0 1) (sqrt 2) .00001)

; (define (distance x1 y1 x2 y2) 0)     ;stub
; (define (distance x1 y1 x2 y2)
;   (... x1 y1 x2 y2))

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1))
           (sqr (- y2 y1)))))


