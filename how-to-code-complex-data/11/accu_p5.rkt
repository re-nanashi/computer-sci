;; (listof Number) -> Number
;; produce product of all elements of lon
(check-expect (product empty) 1)
(check-expect (product (list 2 3 4)) 24)

#;
(define (product lon)
  (cond [(empty? lon) 1]
        [else
         (* (first lon)
            (product (rest lon)))]))

(define (product lon0)
  (local [(define (product lon acc)
            (cond [(empty? lon) acc]
                  [else
                    (product (rest lon)
                             (* acc (first lon)))]))]
    (product lon0 1)))
