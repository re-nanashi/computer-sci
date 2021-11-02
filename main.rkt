;; ListOfNumbers ListOfNumbers -> ListOfNumbers
;; produce a list of absolute differences given 2 list of same length
(check-expect (ablist empty empty) empty)
(check-expect (ablist (list 15 -4 56 10 -23) (list 14 -9 56 14 -23))
              (list 1 5 0 4 0))

;(define (ablist l1 l2) l3)          ;stub
(define (ablist l1 l2)
  (cond [(or (empty? l1) 
             (empty? l2)) empty]
        [else
          (cons (abs (- (first l1) (first l2)))
                (ablist (rest l1) (rest l2)))]))

