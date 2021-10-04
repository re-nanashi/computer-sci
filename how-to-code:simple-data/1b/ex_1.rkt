;; String -> String
;; add "!" t to the end of string
(check-expect (yell "hello") "hello!")
(check-expect (yell "bye") "bye!")

;(define (yell s) "a") ;stub

;(define (yell s) ;template
;  (... s))

(define (yell s)
  (string-append s "!"))
