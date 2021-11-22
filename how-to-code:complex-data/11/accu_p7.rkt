;; (listof Number) -> Number
;; produce sum of all odd numbers of lon
(check-expect (sum-odds empty) 0) 
(check-expect (sum-odds (list 1 2 5 6 11)) 17) 

#;
(define (sum-odds lon)
  (cond [(empty? lon) 0]
        [else
         (if (odd? (first lon))
             (+ (first lon)
                (sum-odds (rest lon)))
             (sum-odds (rest lon)))]))

#;
(define (sum-odds lon0)
  (local [(define (sum-odds lon acc)
            (cond [(empty? lon) acc]
                  [else
                    (sum-odds (rest lon)
                              (if (odd? (first lon))
                                (+ acc (first lon))
                                (+ acc 0)))]))]
    (sum-odds lon0 0)))

(define (sum-odds lon0)
  (local [(define (sum-odds lon acc)
            (cond [(empty? lon) acc]
                  [else
                    (if (odd? (first lon))
                      (sum-odds (rest lon) 
                                (+ acc (first lon)))
                      (sum-odds (rest lon) acc))]))]
    (sum-odds lon0 0)))
