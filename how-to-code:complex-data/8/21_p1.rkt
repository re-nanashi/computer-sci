;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "a" (cons "b" empty)))

;; Functions:

;; ListOfString ListOfString -> ListOfString
;; produce a single list with all the elements of param1 preceding param2
(check-expect (concat empty empty) empty)
(check-expect (concat (list "a") empty) (list "a"))
(check-expect (concat empty (list "b")) (list "b"))
(check-expect (concat (list "a") (list "b")) (list "a" "b"))

;(define (concat los1 los2) los)     ;stub

(define (concat los1 los2)
  (cond [(empty? los1) los2]
        [(empty? los2) los1]
        [else
          (cons (first los1) 
                (concat (rest los1) los2))]))
