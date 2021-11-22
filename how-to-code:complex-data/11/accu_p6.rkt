;; Natural -> Natural
;; produce sum of Natural[0, n]

(check-expect (sum-n 0) 0)
(check-expect (sum-n 1) 1)
(check-expect (sum-n 3) (+ 3 2 1 0))

;(define (sum-n n) 0) ;0

#;
(define (sum-n n)
  (cond [(zero? n) 0]
        [else
         (+ n
            (sum-n (sub1 n)))])) 

(define (sum-n n0)
  (local [(define (sum-n n acc)
            (cond [(zero? n) acc]
                  [else
                    (sum-n (sub1 n)
                           (+ acc n))]))]
    (sum-n n0 0)))
