;; RocketDescent is one of:
;; - Number
;; - false
;; interp. false if rocket's descent has ended, otherwise number of kilometers
;;         left to Earth, restricted to (0, 100]
(define RD1 100)
(define RD2 40)
(define RD3 0.5)
(define RD4 false)

(@dd-template-rules one-of              ;2 cases
                    atomic-non-distinct ;Number
                    atomic-distinct)    ;false
#;
(define (fn-for-rocket-descent rd)
  (cond [(number? rd)
         (... rd)]
         [else  (...)])) 



;; =================
;; Functions:

(@htdf has-landed?)
(@signature RocketDescent -> Boolean)
;; produce true if rocket's descent has ended; false if it's still descending
(check-expect (has-landed? 100) false)
(check-expect (has-landed? 23) false)
(check-expect (has-landed? 0.25) false)
(check-expect (has-landed? false) true)

;(define (has-landed? r) r) ; stub

(@template RocketDescent)

(define (has-landed? rd)
  (cond [(number? rd)
         false]
        [else true])) 
