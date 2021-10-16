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

;; ListOfOdd is one of:
;; - empty
;; (cons Natural[! n % 2 == 0] ListOfOdd)

;; Natural -> ListOfOdd
;; produces a list of all odd numbers from to 1
(check-expect (oddNums 0) empty)
(check-expect (oddNums 3) (cons 3 (cons 1 empty)))
(check-expect (oddNums 5) (cons 5 (cons 3 (cons 1 empty))))

;(define (oddNums n) 1)  ;stub

#;
(define (oddNums n)   
  (cond [(zero? n) empty]
        [else
          (cond [(odd? n) 
                 (cons n (oddNums (sub1 n)))]
                [else 
                  (oddNums (sub1 n))])]))

(define (oddNums n)   
  (cond [(zero? n) empty]
        [else
          (if (odd? n) (cons n (oddNums (sub1 n)))
            (oddNums (sub1 n)))]))
