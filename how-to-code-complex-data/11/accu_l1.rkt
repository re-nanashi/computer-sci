;; (listof X) N -> (listof X)
;; produce list by including first element then skipping n elements
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "d"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 1) (list "a" "c" "e"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 3) (list "a" "e"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 0) (list "a" "b" "c" "d" "e" "f"))

(define (skipn lox0 n)
  (local [(define (fn-for-lox lox acc)
            (cond [(empty? lox) empty]
                  [else
                    (cond [(equal? acc 0) 
                           (cons (first lox)
                                 (fn-for-lox (rest lox) n))]
                          [else 
                            (fn-for-lox (rest lox) (sub1 acc))])]))]
    (fn-for-lox lox0 0)))
