;; (listof X) -> (listof X)
;; produce list with elements of lox in reverse order
(check-expect (rev empty) empty)
(check-expect (rev (list 1)) (list 1))
(check-expect (rev (list "a" "b" "c")) (list "c" "b" "a"))

;(define (rev lox) empty)   ; stub

#;
(define (rev lox)                    
  (cond [(empty? lox) (...)]
        [else
         (... (first lox)
              (rev (rest lox)))]))

(define (rev lox0)
  (local [(define (rev lox acc)
            (cond [(empty? lox) acc]
                  [else
                    (rev (rest lox)
                         (cons (first lox) 
                               acc))]))]
    (rev lox0 empty)))
