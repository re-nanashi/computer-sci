;; (listof X) N -> (listof X)
;; produce a list where each element is replicated n times
(check-expect (replicate-elm (list "a" "b" "c") 2) (list "a" "a" "b" "b" "c" "c"))

(define (replicate-elm lox0 n)
  (local [(define (replicate-elm lox acc)
            (cond [(empty? lox) empty]
                  [else
                    (if (< acc n)
                      (cons (first lox)
                            (replicate-elm lox (add1 acc)))
                      (replicate-elm (rest lox) 0))]))]
    (replicate-elm lox0 0)))
