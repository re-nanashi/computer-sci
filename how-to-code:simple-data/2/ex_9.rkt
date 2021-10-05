;; DinnerOrder is one of:
;; - "chicken"
;; - "pasta"
;; - false
;; interp. dinner options for each passenger, false means no dinner

(define DO1 "chicken")
(define DO2 "pasta")
(define DO3 false)

#;
(define (fn-for-dinner-order d) 
    (cond [(string? d) (... d)] 
          [(false? d) (...)]))

;; Template rules used:
;; - one of: 3 cases
;; - atomic non-distinct: "Chicken"
;; - atomic non-distinct: "Pasta"
;; - atomic distinct: false

;; DinnerOrder -> String
;; produce a message saying what the passenger ordered
(check-expect (dinnerOrderMessage DO1) "The passenger ordered chicken.")
(check-expect (dinnerOrderMessage DO2) "The passenger ordered pasta.")
(check-expect (dinnerOrderMessage DO3) "The passenger did not order.")

;(define (dinnerOrderMessage d) false)      ;stub
;; <Template from DinnerOrder>

(define (dinnerOrderMessage d) 
    (cond [(string? d) (string-append "The passenger ordered " d ".")] 
          [(false? d) "The passenger did not order."]))

