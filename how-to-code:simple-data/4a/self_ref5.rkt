;; =================
;; Data definitions:

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; interp. list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
          (... (first lon)
               (fn-for-lon (rest lon)))]))

;; Template rules used:
;; -one of: 2 cases
;; -atomic distinct: empty
;; -compound: (cons Number ListOfNumber)
;; -self-reference: (rest lon) is ListOfNumber

;; =================
;; Functions:

;; ListOfNumber -> Number
;; produces the largest number in the list
(check-expect (largestNumber LON1) 0)
(check-expect (largestNumber LON2) 60)

;(define (largestNumber lon) 0) ;stub

(define (largestNumber lon)
  (cond [(empty? lon) 0]
        [else
          (if (> (first lon) (largestNumber (rest lon))) 
              (first lon)
              (largestNumber (rest lon)))]))
