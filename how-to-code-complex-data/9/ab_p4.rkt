;; (Number -> Boolean) (listof Number) -> Boolean
;; produce true if some number in lon is pred
(check-expect (some_pred positive? empty) false)
(check-expect (some_pred positive? (list 2 -3 -4)) true)
(check-expect (some_pred negative? (list 2 3 -4)) true)
(check-expect (some_pred negative? (list 2 3 4)) false)
(check-expect (some_pred zero? (list 2 3 4)) false)

(define (some_pred pred lon)
  (cond [(empty? lon) false]
        [else
          (or (pred (first lon))
              (some_pred pred (rest lon)))]))

;; ListOfNumber -> Boolean
;; produce true if some number in lon is positive
(check-expect (some-positive? empty) false)
(check-expect (some-positive? (list 2 -3 -4)) true)
(check-expect (some-positive? (list -2 -3 -4)) false)

(define (some-positive? lon)
  (some_pred positive? lon))


;; ListOfNumber -> Boolean
;; produce true if some number in lon is negative
(check-expect (some-negative? empty) false)
(check-expect (some-negative? (list 2 3 -4)) true)
(check-expect (some-negative? (list 2 3 4)) false)

(define (some-negative? lon)
  (some_pred negative? lon))
