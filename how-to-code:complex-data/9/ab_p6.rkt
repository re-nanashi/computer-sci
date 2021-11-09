(define-struct bag (l w h))
;; Bag is (make-bag Number Number Number)
;; interp. a bag with a length, width and height in centimeters 
(define B1 (make-bag 19.5 10.0 6.5))
(define B2 (make-bag 23.0 11.5 7.0))
(define B3 (make-bag 18.0 9.5 5.5))

;; ListOfBag is one of:
;; - empty
;; - (cons Bag ListOfBag)
;; interp. a list of bags
(define LOB1 empty)
(define LOB2 (list B1 B2 B3))

;; ListOfBag -> ListOfNumbers
;; produce the linear length of each bag in the least
(check-expect (linear_length_lob LOB1) empty)
(check-expect (linear_length_lob LOB2) (list (+ 19.5 10.0 6.5)
                                             (+ 23.0 11.5 7.0)
                                             (+ 18.0 9.5 5.5)))

;(define (linear_length_lob lob) empty)  ; stub

(define (linear_length_lob lob)
  (local [(define (get_linear_length b) 
            (+ (bag-l b) (bag-w b) (bag-h b)))]
    (map get_linear_length lob)))
