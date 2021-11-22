(define-struct counts (odds evens))
;; Counts is (make-counts Natural Natural)
;; interp. describes the number of even and odd numbers in a list

(define C1 (make-counts 0 0)) ;describes an empty list
(define C2 (make-counts 3 2)) ;describes (list 1 2 3 4 5))

;; (listof Numbers) -> Counts
;; produce Counts for a given a list of numbers.
(check-expect (count_odd_even (list 1 2 3 4 5)) (make-counts 3 2))

;; Solution #1: 2 accu
#;
(define (count_odd_even lon0)
  (local [(define (count_odd_even lon acc1 acc2)
            (cond [(empty? lon) (make-counts acc1 acc2)]
                  [else
                    (if (odd? (first lon))
                      (count_odd_even (rest lon)
                                      (+ acc1 1)
                                      acc2)
                      (count_odd_even (rest lon)
                                      acc1
                                      (+ acc2 1)))]))]
    (count_odd_even lon0 0 0)))

;; Solution #2: 1 accu
(define (count_odd_even lon0)
  (local [(define (count_odd_even lon acc)
            (cond [(empty? lon) acc]
                  [else
                    (if (odd? (first lon))
                      (count_odd_even (rest lon) 
                                      (make-counts (+ (counts-odds acc) 1) 
                                                   (counts-evens acc))) 
                      (count_odd_even (rest lon) 
                                      (make-counts (counts-odds acc) 
                                                   (+ (counts-evens acc) 1))))]))]
    (count_odd_even lon0 (make-counts 0 0))))
