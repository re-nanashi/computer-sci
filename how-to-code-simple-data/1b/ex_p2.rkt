;; String -> Boolean
;; produces true if string length is less than 5
(check-expect (isLessThanFive? "Test") true)
(check-expect (isLessThanFive? "Test function") false)
(check-expect (isLessThanFive? "seven") false)

;;(define (isLessThanFive? str)      ;;stub
;;  false)

;;(define (isLessThanFive? str)      ;;template
;;  (...str))

(define (isLessThanFive? str)
  (< (string-length str) 5))
