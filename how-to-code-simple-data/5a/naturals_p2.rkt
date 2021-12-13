;; Natural is one of:
;; - zero
;; - (add1 Natural)
;; interp. a natural number
(define N0 0)
(define N1 (add1 N0))
(define N2 (add1 N1))

#; 
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
          (... n (fn-for-natural (sub1 n)))]))

;; Template rules used:
;; - one of: two cases
;; - atomic distinct: 0
;; - compound: (add1 Natural)
;; - self-reference: (sub1 n) is Natural

;; Natural -> Image
;; produces an image of all the numbers from n to 0 side by side
(check-expect (decreasingImage 0) (text "0" 24 "black"))

(check-expect (decreasingImage 1) (beside (text "1" 24 "black")
                                          (text "0" 24 "black")))

(check-expect (decreasingImage 2) (beside (text "2" 24 "black")

                                          (text "1" 24 "black")
                                          (text "0" 24 "black")))

;(define (decreasingImage n) (square 0 "solid" "white")) ;stub

(define (decreasingImage n)
  (cond [(zero? n) (text "0" 24 "black")]
        [else
          (beside (text (number->string n) 24 "black") 
                  (decreasingImage (sub1 n)))]))
#;
(define (decreasingImage n)
  (cond [(< n 0) (square 0 "solid" "white")]
        [else
          (beside (text (number->string n) 24 "black") 
                  (decreasingImage (sub1 n)))]))
