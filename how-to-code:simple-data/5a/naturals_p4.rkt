;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; interp. a natural number
(define N0 0)
(define N1 (add1 N0))
(define N2 (add1 N1))

#; 
(define (fn-for-natural n)   ; template
  (cond [(zero? n) (...)]
        [else
          (... n 
               (fn-for-natural (sub1 n)))]))

;; Template rules used:
;; - one of: two cases
;; - atomic distinct: 0
;; - compound: (add1 Natural)
;; - self-reference: (sub1 n) is Natural

;; Natural Color<string> -> Image
;; produces n concentric circles with the given color
(check-expect (draw 0 "black") empty-image)
(check-expect (draw 1 "black") (overlay (circle 10 "outline" "red")
                                         empty-image))
(check-expect (draw 2 "red") (overlay (circle 20 "outline" "red")
                                      (circle 10 "outline" "red")
                                      empty-image))

;(define (draw 0 "black") (square 0 "solidl "white")) ;stub

(define (draw n c) 
  (cond [(zero? n) empty-image]
        [else
          (overlay (circle (* 10 n) "outline" c)
                   (draw (sub1 n) c))]))
