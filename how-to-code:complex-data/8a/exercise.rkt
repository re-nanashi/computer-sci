;; ListOfNumber -> ListOfNumber
;; sort the numbers in lon in increasing order
(check-expect (sort_list empty) empty)
(check-expect (sort_list (list 1)) (list 1))
(check-expect (sort_list (list 1 2 3)) (list 1 2 3))
(check-expect (sort_list (list 2 1 3)) (list 1 2 3))
(check-expect (sort_list (list 3 2 1)) (list 1 2 3))

(define (sort_lon lon)
  (cond [(empty? lon) empty]
        [else
          (insert (first lon) 
                  (sort_lon (rest lon)))]))

;; Number ListOfNumber -> ListOfNumber
;; insert n in proper position in lon
;; ASSUME: lon is sorted in increasing order
(check-expect (insert 2 empty) (list 2))
(check-expect (insert 2 (list 1 3)) (list 1 2 3))

(define (insert n lon)
  (cond [(empty? lon) (cons n empty)]
        [else
          (if (> (first lon) n)
            (cons n lon)
            (cons (first lon) (insert n (rest lon))))]))

(define (sort_lst lon)
  (local [
          (define (sort_lon lon) 
            (cond [(empty? lon) empty] 
                  [else 
                    (insert (first lon) 
                            (sort_lon (rest lon)))]))
          (define (insert n lon) 
            (cond [(empty? lon) (cons n empty)] 
                  [else 
                    (if (> (first lon) n) 
                      (cons n lon) 
                      (cons (first lon) (insert n (rest lon))))]))] 
    (sort_lon lon)))
