;; Natural is one of:
;; - 0
;; - (add1 Natural)
;; ;; interp. a natural number
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

;; Natural -> Natural
;; produces the sum of all natural from 0 to the given n
(check-expect (sum 0) 0)
(check-expect (sum 2) (+ 2 1 0))
(check-expect (sum 3) (+ 3 2 1 0)) 
(check-expect (sum 4) (+ 4 3 2 1 0)) 


;(define (sum n) 0)      ; stub

(define (sum n)   
  (cond [(zero? n) 0]
        [else
          (+ n (sum (sub1 n)))]))
