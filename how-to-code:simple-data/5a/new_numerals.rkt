;; NATURAL is one of:
;; - empty
;; - (cons "!" NATURAL)
;; interp. a natural number, the number of "!" in the list is the number
(define N0 empty)               ; 0
(define N1 (cons "!" N0))       ; 1
(define N2 (cons "!" N1))       ; 2
(define N3 (cons "!" N2))       ; 3
(define N4 (cons "!" N3))       ; 4
(define N5 (cons "!" N4))       ; 5
(define N6 (cons "!" N5))       ; 6
(define N7 (cons "!" N6))       ; 7
(define N8 (cons "!" N7))       ; 8
(define N9 (cons "!" N8))       ; 9

;; These are the primitives that operate NATURAL:
(define (ZERO? n) (empty? n))   ; Any -> Boolean
(define (ADD1 n) (cons "!" n))  ; Natural -> Natural
(define (SUB1 n) (rest n))      ; Natural[>0] -> Natural

#; <template>
(define (fn-for-NATURAL n)
  (cond [(ZERO? n) (...)]
        [else
          (... n
               (fn-for-NATURAL (SUB1 b)))])) 

;; NATURAL NATURAL -> NATURAL
;; produce a + b
(check-expect (ADD N2 N0) N2)
(check-expect (ADD N0 N3) N3)
(check-expect (ADD N3 N4) N3)

;(define (ADD a b) N0) ;stub

(define (ADD a b)
  (cond [(ZERO? b) a]
        [else 
          (ADD (ADD1 a) (SUB1 b))])) 

;; NATURAL NATURAL -> NATURAL
;; produce a - b
(check-expect (SUB N2 N0) N2)
(check-expect (SUB N1 N0) N1)
(check-expect (SUB N2 N1) N1)
(check-expect (SUB N5 N2) N3)


;(define (SUB a b) N0) ;stub
(define (SUB a b)
  (cond [(ZERO? b) a]
        [else 
          (SUB (SUB1 a) (SUB1 b))]))
