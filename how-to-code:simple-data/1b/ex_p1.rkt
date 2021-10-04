;; String -> String
;; produces a summoning charm by appending given string to "accio"
(check-expect (summon "Firebolt") "accio Firebolt")
(check-expect (summon "portkey") "accio portkey")
(check-expect (summon "broom") "accio broom")

;;(define (summon str)      ;;stub 
;;  "a")

;;(define (summon str)      ;;template
;;  (... str))

(define (summon str)
  (string-append "accio " str))
