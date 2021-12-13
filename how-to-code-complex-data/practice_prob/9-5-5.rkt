;;Convert:
;; A digit is in 0, ..., 9.
;; ListOfDigit is one of:
;; - empty
;; - (cons Digit ListOfDigit)
;; interp. a list of digits

;; ListOfDigit -> Number
;; convert a vector of digits into a number
;; 0 digit is the right-most digit in number,
;; N-th digit is the left-most digit in number
(check-expect (convert (cons 1 (cons 2 (cons 3 empty)))) 321)
(check-expect (convert (cons 4 (cons 5 (cons 6 empty)))) 654)

;(define (convert lod) 0)    ;stub

(define (convert lod)
  (cond [(empty? lod) 0]
        [else
            (+ (first lod) 
               (* (convert (rest lod) 10)))]))

