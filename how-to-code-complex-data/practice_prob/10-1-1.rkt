;; hours_wages : ListOfNumbers -> ListOfNumbers
;; create a list of weekly wages from a list of weekly hours

#;
(define (hours_wages lon)
  (cond [(empty? lon) (...)]
        [else 
          (... (first lon) 
               (hours_wages (rest lon)))]))

(define (hours_wages lon)
  (cond [(empty? lon) 0] 
        [else 
          (cond [(> (first lon) 100) 
                 (error "too many hours.")] 
                [else 
                  (cons (* 14 (first lon)) 
                        (hours_wages (rest lon)))])]))
