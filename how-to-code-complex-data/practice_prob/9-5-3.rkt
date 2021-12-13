;; ListOfPrices is one of:
;; - empty
;; - (cons Price ListOfPrices)
;; interp. a list of prices

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else 
          (... (first lop) 
               (fn-for-lop (rest lop)))]))

;; ListOfPrices -> Boolean
;; check whether all of the prices are below 1
(check-expect (dollar_store? empty) true)
(check-expect (dollar_store? (cons .75 (cons 1.95 (cons .25 empty)))) false)
(check-expect (dollar_store? (cons .15 (cons .05 (cons .25 empty)))) true)

;(define (dollar_store? lop) empty)  ; stub
; <template from ListOfPrices>

(define (dollar_store? lop)
  (cond [(empty? lop) true]
        [else 
          (and (<= (first lop) 1) 
               (dollar_store? (rest lop)))]))

;; ListOfPrices -> Number
;; calculate the average price 
(check-expect (average (cons 1.00 empty)) 1.00)
(check-expect (average (cons 0.50 (cons 1.50 empty))) 1.00)
(check-expect (average (cons 1.25 (cons 3.75 (cons 4.00 empty)))) 3.00)

(define (average lop)
  (/ (sum lop)
     (count lop)))

(define (sum lop)
  (cond [(empty? lop) 0]
        [else
          (+ (first lop)
             (sum (rest lop)))]))

(define (count lop)
  (cond [(empty? lop) 0]
        [else
          (+ 1 (count (rest lop)))]))
