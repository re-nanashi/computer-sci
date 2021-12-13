;; (listof String) -> (listof String)
;; append each string's position in the list to the front
;; of the string to number the list
(check-expect (number_list empty) empty)
(check-expect (number_list (list "first" "second" "third")) 
              (list "1: first" "2: second" "3: third"))
(check-expect (number_list (list "first" "second" "third" "fourth")) 
              (list "1: first" "2: second" "3: third" "4: fourth"))
;(define (number_list los) los)     ; stub

(define (number_list los0)
  (local [(define (fn-for-lst los acc)
            (cond [(empty? los) empty]
                  [else
                    (cons (string-append (number->string acc) ": " (first los))
                          (fn-for-lst (rest los) (add1 acc)))]))]
    (fn-for-lst los0 1)))
