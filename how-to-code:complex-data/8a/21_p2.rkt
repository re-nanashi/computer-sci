;; ListOfNumbers ListOfNumbers -> ListOfNumbers
;; produce a single list of all the numbers in ascending order.
(check-expect (merge empty empty) empty)
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge empty (list 2)) (list 2))
(check-expect (merge (list 2) (list 1)) (list 1 2))
(check-expect (merge (list 1) (list 2)) (list 1 2))
(check-expect (merge (list 1 2 3) (list 2)) (list 1 2 2 3))

;(define (merge lon1 lon2) lon3)     ; stub

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
          (sort (cons (first lon1)
                      (merge (rest lon1) lon2)) <)]))

(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else
          (if (<= (first l1) (first l2))
            (cons (first l1) 
                  (merge (rest l1) l2))
            (cons (first l2)
                  (merge l1 (rest l2))))]))
