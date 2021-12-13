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

;; ListOfNatural is one of:
;; - empty
;; - (cons Natural(0, n] ListOfNatural)
;; interp. list of natural numbers
(define LON0 empty)
(define LON1 (cons 1 empty))
(define LON2 (cons 2 (cons 1 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) empty]
        [else
          (... (first lon)
               (fn-for-lon (rest lon)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Natural(0, n] ListOfNatural)
;; - self-reference: (rest lon) is ListOfNatural

;; Natural -> Natural
;; produce a the sum of all natural in Natural[0, n]
(check-expect (sum 0) 0) ; base case
(check-expect (sum 1) (+ 1 0))
(check-expect (sum 3) (+ 3 2 1 0))

;(define (sum n) 0) ;stub

(define (sum n)   
  (cond [(zero? n) 0]
        [else
          (+ n (sum (sub1 n)))]))   ; (+ 3 2 1 0)

;; Natural -> ListOfNatural
;; produces a list of all naturals leading up to n
(check-expect (createList 0) empty)
(check-expect (createList 1) (cons 1 empty))
(check-expect (createList 2) (cons 2 (cons 1 empty)))
(check-expect (createList 3) (cons 3 (cons 2 (cons 1 empty))))

;(define (createList n) (cons n empty)) ; stub

(define (createList n)   ; template
  (cond [(zero? n) empty]
        [else
          (cons n 
                (createList (sub1 n)))]))
