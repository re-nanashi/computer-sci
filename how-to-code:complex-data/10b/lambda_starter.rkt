;; Number (listof Number) -> (listof Number)
;; produce only elements of lon > threshold
(check-expect (only-bigger 2 empty) empty)
(check-expect (only-bigger 3 (list 2 4 5)) (list 4 5))

(define (only-bigger threshold lon)
  (filter (lambda (n) (> n threshold)) lon))


;; (listof Image) -> (listof Natural)
;; produce list of areas of images
(check-expect (all-areas (list (rectangle 2 3 "solid" "blue") 
                               (square 10 "solid" "white")))
              (list 6 100))

(define (all-areas loi)
  (map (lambda (i) (* (image-width i) (image-height i))) loi))


;; (listof Number)  ->  (listof Number)
;; produce list of numbers sorted in ASCENDING order
;; ASSUMPTION: lon contains no duplicates
(check-expect (qsort empty)                empty)
(check-expect (qsort (list 8))             (list 8))
(check-expect (qsort (list 7 8 9))         (list 7 8 9))
(check-expect (qsort (list  4 3 2 1))      (list 1 2 3 4))
(check-expect (qsort (list 6 8 1 9 3 7 2)) (list 1 2 3 6 7 8 9))

(define (qsort lon)
  (cond [(empty? lon) empty]
        [else
          (local [(define p (first lon))]
            (append (qsort (filter (lambda (n) (< n p)) lon))
                    (list p)
                    (qsort (filter (lambda (n) (> n p)) lon))))]))

