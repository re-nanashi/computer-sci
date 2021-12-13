;; ==============
;; Data definitions:

;; ListOfNumber is one of:
;; - empty 
;; - (cons Number ListOfNumber) 
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
          (... (first lon)
               (fn-for-lon (rest lon)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Number ListOfNumber)
;; - self-reference: (rest lon) is ListOfNumber

;; ==============
;; Functions:

;; ListOfNumber -> ListOfNumber
;; doubles every number in the list
(check-expect (double LON1) empty)
(check-expect (double LON2) (cons 120 (cons 84 empty)))
(check-expect (double (cons 2 empty)) (cons 4 empty))

;(define (double lon) empty) ;stub

(define (double lon)
  (cond [(empty? lon) empty]
        [else
          (cons (* 2 (first lon))
               (double (rest lon)))]))

