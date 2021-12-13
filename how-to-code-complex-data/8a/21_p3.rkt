;; Data definitions:

(define-struct entry (k v))
;; Entry is (make-entry Number Number)
;; interp. an entry maps a key to a value
(define E1 (make-entry 3 12))

;; ListOfEntry is one of:
;; - empty
;; - (cons Entry ListOfEntry)
;; interp. a list of key value entries
(define LOE1 (list E1 (make-entry 1 11)))

;; ListOfNumbers ListOfNumbers -> ListOfEntry
;; produce a list of entries given 2 lists
(check-expect (zip empty empty) empty)
(check-expect (zip (list 1) empty) empty)
(check-expect (zip empty (list 1)) empty)
(check-expect (zip (list 1 2) (list 11 12)) (list (make-entry 1 11) (make-entry 2 12)))

;(define (zip l1 l2) loe)        ; stub

(define (zip l1 l2)
  (cond [(or (empty? l1) 
             (empty? l2)) empty]
        [else
          (cons (make-entry (first l1) (first l2))
                (zip (rest l1) (rest l2)))]))

