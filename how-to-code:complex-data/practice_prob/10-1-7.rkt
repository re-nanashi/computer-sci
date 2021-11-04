;; String ListOfStrings -> ListOfStrings
;; remove the instances of given String in the list then produce new list
(check-expect (remov "robot" (cons "robot" (cons "doll" (cons "dress" empty))))
              (cons "doll" (cons "dress" empty)))

(define (remov str los)
  (cond [(empty? los) empty]
        [else
          (if (string=? str (first los))
            (remov str (rest los))
            (cons (first los)
                  (remov str (rest los))))]))
