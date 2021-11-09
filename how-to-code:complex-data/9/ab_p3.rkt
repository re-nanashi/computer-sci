;; (X -> Number) (listof X) -> Number
;; produce the sum of fn on every X in the listof X
(check-expect (sum_of sqr empty) 0)
(check-expect (sum_of sqr (list 2 4)) (+ 4 16))
(check-expect (sum_of string-length empty) 0)
(check-expect (sum_of string-length (list "a" "bc")) 3)

;(define (sum_of fn lst) 0)     ; stub

(define (sum_of fn lst)
  (cond [(empty? lst) 0] 
        [else
          (+ (fn (first lst))
             (sum_of fn (rest lst)))]))

;; (listof Number) -> Number
;; produce the sum of the squares of the numbers in lon
(check-expect (sum-of-squares empty) 0)
(check-expect (sum-of-squares (list 2 4)) (+ 4 16))

(define (sum-of-squares lon)
  (sum_of sqr lon))

;; (listof String) -> Number
;; produce the sum of the lengths of the strings in los
(check-expect (sum-of-lengths empty) 0)
(check-expect (sum-of-lengths (list "a" "bc")) 3)

(define (sum-of-lengths los)
  (sum_of string-length los))
