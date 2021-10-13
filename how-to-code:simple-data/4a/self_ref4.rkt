;; ==============
;; Data definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los)(...)]
        [else
          (... (first los)
               (fn-for-los (rest los)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfString)
;; - self-reference: (rest los) is ListOfString

;; ==============
;; Functions

;; ListOfString -> ListOfString
;; add ! to the end of each string inside ListOfString
(check-expect (yell LS0) empty)
(check-expect (yell LS1) (cons "a!" empty))
(check-expect (yell LS2) (cons "a!" (cons "b!" empty)))
(check-expect (yell LS3) (cons "c!" (cons "b!" (cons "a!" empty))))

;(define (yell los) empty) ;stub

(define (yell los)
  (cond [(empty? los) empty]
        [else
          (cons (string-append (first los) "!")
                (yell (rest los)))]))
