;;(listof Number) -> Number
;; produce the average of the numbers in the given list
(check-expect (average (list 25 45 65)) 45)
(check-expect (average (list 5 9 8 6)) 7)

(define (average lon0) 
  ;; acc: Natural
  ;; (average (25 45 65) 0 0)

  ;; (average (25 45 65) 0   0)
  ;; (average (   45 65) 25  1)
  ;; (average (      65) 70  2)
  ;; (average (        ) 135 3)
  (local [(define (average lon acc1 acc2)
            (cond [(empty? lon) (/ acc1 acc2)]
                  [else
                    (average (rest lon)
                             (+ acc1 (first lon))
                             (+ acc2 1))]))]
    (average lon0 0 0)))
