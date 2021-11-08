;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

; (define (positive-only lon) empty)    ; stub

(define (positive-only lon)
  (filter2 lon positive?))

;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

; (define (negative-only lon) empty)    ; stub

(define (negative-only lon)
  (filter2 lon negative?))

;; (X -> Boolean) (listof X) -> (listof X)
;; given a list and a fn, call fn to all items on the list.
(check-expect (filter2 empty negative?) empty)
(check-expect (filter2 (list 1 -2 3 -4) positive?) (list 1 3))
(check-expect (filter2 (list 1 -2 3 -4) negative?) (list -2 -4))

(define (filter2 lon fn)
  (cond [(empty? lon) empty]
        [else
          (if (fn (first lon))
            (cons (first lon)
                  (filter2 (rest lon) fn))
            (filter2 (rest lon) fn))]))
