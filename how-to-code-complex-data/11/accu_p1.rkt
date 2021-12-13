;; (listof X) N -> (listof X)
;; produce a list formed by dropping every nth element
(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (dropn (list 1 2 3 4 5 6 7) 1) (list 1 3 5 7))
(check-expect (dropn (list 1 2 3 4 5 6 7) 0) empty)

(define (dropn lox0 n)
  (local [(define (dropn lox acc)
            (cond [(empty? lox) empty]
                  [else
                    (if (equal? acc n)
                      (dropn (rest lox) 0)
                      (cons (first lox) 
                            (dropn (rest lox) (add1 acc))))]))]
    (dropn lox0 0)))
