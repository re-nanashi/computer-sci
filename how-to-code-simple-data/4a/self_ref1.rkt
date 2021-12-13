;; ==================
;; Data definitions:

;; ListOfStrings is one of:
;; - empty
;; (cons String ListOfStrings)
;; interp. list of strings
(define LOS0 empty)
(define LOS1 (const "a" empty))
(define LOS1 (const "a" (const "b" empty)))
(define LOS1 (const "a" (const "b" (const "c" empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else 
          (... (first los)
               (fn-for-los (rest los)))]))

;; Template rules used: 
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - atomic non-distinct: (first los) is  String
;; - self-reference: (rest los) is ListOfString

;; =================
;; Functions:

;; ListOfStrings -> Natural
;; produce the total number of characters in the list
(check-expect (totalNumberOfChar LOS0) 0)
(check-expect (totalNumberOfChar LOS1) 1)
(check-expect (totalNumberOfChar LOS2) (+ 1 (+ 1 0)))
(check-expect (totalNumberOfChar LOS3) (+ 1 (+ 1 (+ 1 0))))

;(define (totalNumberOfChar los) 0) ;stub

(define (totalNumberOfChar los)
  (cond [(empty? los) 0]
        [else 
          (+ (string-length (first los))
             (totalNumberOfChar (rest los)))]))
