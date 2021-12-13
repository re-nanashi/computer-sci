;; (listof N) -> Boolean
;; produce true if N in list is strictly decreasing
(check-expect (strictly_decreasing? (list 2 1)) true)
(check-expect (strictly_decreasing? (list 1 2)) false)
(check-expect (strictly_decreasing? (list 5 4 3 2 1)) true)
(check-expect (strictly_decreasing? (list 5 4 1 2 1)) false)

(define (strictly_decreasing? lon0)
  (local [(define (strictly_decreasing? lon prev)
            (cond [(empty? lon) true]
                  [else
                    (if (< (first lon) prev)
                      (strictly_decreasing? (rest lon) (first lon))
                      false)]))]
    (strictly_decreasing? (rest lon0) (first lon0))))
