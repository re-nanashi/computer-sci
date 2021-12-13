;; ListOfPrices ua -> ListOfPrices
;; given a list, produce a list of all prices that are below ua
(check-expect (inexpensive_toys empty 1) empty)
(check-expect (inexpensive_toys (cons 2.95 (cons .95 (cons 1.0 (cons 5 empty)))) 1.0) 
              (cons .95 (cons 1.0 empty)))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
          (... (first lop)
               (fn-for-lop (rest lop)))]))

(define (inexpensive_toys lop ua)
  (cond [(empty? lop) empty]
        [else
          (if (not (<= (first lop) ua)) 
            (inexpensive_toys (rest lop) ua)
            (cons (first lop) 
                  (inexpensive_toys (rest lop) ua)))]))
