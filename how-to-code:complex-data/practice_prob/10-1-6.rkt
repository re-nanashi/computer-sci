;; String String ListOfStrings -> ListOfStrings
;; replace param2 on the list to param1 if found, otherwise retain
(check-expect (replace "Barbie" "doll" (cons "robot" (cons "doll" (cons "dress" empty))))
              (cons "robot" (cons "Barbie" (cons "dress" empty))))
(check-expect (replace "Barbie" "doll" (cons "doll" (cons "doll" (cons "dress" empty))))
              (cons "Barbie" (cons "Barbie" (cons "dress" empty))))

(define (replace str stc los)
  (cond [(empty? los) empty]
        [else
          (if (string=? stc (first los))
            (cons str
                  (replace str stc (rest los)))
            (cons (first los)
                  (replace str stc (rest los))))]))
