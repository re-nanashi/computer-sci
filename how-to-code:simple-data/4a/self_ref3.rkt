;; ==============
;; Data definitions:

;; ListOfBoolean is one of:
;; - empty
;; - (const Boolean ListOfBoolean)
;; interp. a list of boolean
(define LOB1 empty)
(define LOB2 (cons true (cons false empty)))
(define LOB3 (cons true (cons true empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob)(...)]
        [else
          (... (first lob)
               (fn-for-lob (rest lon)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Boolean ListOfBoolean)
;; - self-reference: (rest lob) is ListOfBoolean

;; ==============
;; Functions:

;; ListOfBoolean -> ListOfBoolean
;; produces true if every value in the list is true and the list is empty, else false
(check-expect (allIsTrue LOB1) true)
(check-expect (allIsTrue LOB2) false)
(check-expect (allIsTrue LOB3) true)

;(define (allIsTrue lob) false) ;stub

(define (allIsTrue lob)
  (cond [(empty? lob) true]
        [else
          (equal? (first lob)
               (allIsTrue (rest lon)))]))
#;
(define (allIsTrue lob)
  (cond [(empty? lob) true]
        [else
          (and (first lob)
               (allIsTrue (rest lon)))]))
