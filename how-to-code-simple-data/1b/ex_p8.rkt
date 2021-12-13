;; String -> String
;; produce a question by appending "?" to the given string
(check-expect (createQuestion "How are you") "How are you?") 
(check-expect (createQuestion "How old are you") "How old are you?") 
(check-expect (createQuestion "How old are you?") "How old are you?") 

;;(define (createQuestion str)        ;;stub
;;  "a")

;;(define (createQuestion str)        ;;template
;;  (...str))

(define (createQuestion str)
  (if (string=? (substring str (- (string-length str) 1)) "?")
      str
      (string-append str "?")))

