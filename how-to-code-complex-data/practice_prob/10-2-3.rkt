(define-struct ir (name price))
;; InventoryRecord is (make-ir String Number)
;; interp. InventoryRecord has name of the item and it's price

;; Inventory is one of:
;; - empty
;; - (cons InventoryRecord Inventory)
;; interp. a list of InventoryRecords

#;
(define (fn-for-inv inv)
  (cond [(empty? inv) (...)]
        [else 
          (... (first inv)
               (fn-for-inv (rest inv)))]))

;; price_of : String Inventory -> Number or String
;; produce the price of the given string, if not found, produce not found
(check-expect (price_of "rocket" empty) "Not found.")
(check-expect (price_of "rocket" (cons (make-ir "rocket" 3.22) empty)) 3.22)
(check-expect (price_of "rocket" (cons (make-ir "doll" 1.21) (cons (make-ir "rocket" 3.22) empty))) 3.22)

;(define (price_of n inv) 0)         ; stub

(define (price_of n inv)
  (cond [(empty? inv) "Not found."]
        [else 
          (if (string=? n (ir-name (first inv)))
            (ir-price (first inv)) 
            (price_of n (rest inv)))]))
