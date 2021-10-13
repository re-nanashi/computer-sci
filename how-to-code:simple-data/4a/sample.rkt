;; Data definitions:

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; interp. each number in the list is an owl weight in ounces
(define LON1 empty)
(define LON2 (cons 60 (cons 42 empty)))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
          (... (first lon)
               (fn-for-lon (rest lon)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Number ListOfNumber)

;; Functions: 

;; ListOfNumber -> Number
;; produce total weight of owls in consumed list
(check-expect (sum empty) 0)
(check-expect (sum (cons 60 empty)) (+ 60 0))
(check-expect (sum (cons 60 (cons 42 empty))) (+ 60 (+ 42 0)))

(define (sum lon) 0) ; stub

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
          (+ (first lon)
             (sum (rest lon)))]))

;; ListOfNumber -> Natural
;; prduce the total number of owls in consumed list
(check-expect (totalNumberOfOwls empty) 0)
(check-expect (totalNumberOfOwls (cons 12 empty)) (+ 1 0))
(check-expect (totalNumberOfOwls (cons 60 (cons 42 empty))) (+ 1 (+ 1 0)))

; (define (totalNumberOfOwls lon) 0) ;stub

(define (totalNumberOfOwls lon)
  (cond [(empty? lon) 0]
        [else
          (+ 1 (totalNumberOfOwls (rest lon)))]))
