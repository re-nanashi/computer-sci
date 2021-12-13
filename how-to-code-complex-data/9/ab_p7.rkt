;; Natural -> Natural
;; produce the sum of the list first n odd numbers
(check-expect (sum_n_odds 0) 0)
(check-expect (sum_n_odds 1) (+ 0 1))
(check-expect (sum_n_odds 3) (+ 0 1 3 5))

;; (define (sum_n_odds n) 0)  ; stub

(define (sum_n_odds n) 
  (local [(define (pred n) (odd? n))]
    (foldr + 0 (filter pred (build-list (* n 2) add1)))))
