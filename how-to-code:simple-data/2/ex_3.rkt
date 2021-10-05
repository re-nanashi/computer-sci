;; RocketDescent is one of:
;; - Number(0, 100]
;; - "touchdown"
;; interp. distance in kilometers from space to earth
;;   Number(0, 100]   is distance until touchdown
;;   "touchdown"    rocket has succesfully landed on earth

(define rocketDescent1 50)
(define rocketDescent2 "touchdown")

#;
(define (rocketDescentFunc rd)
    (cond [(number? rd) (...)]
          [else (...)]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic non-distinc: Number(0, 100]
;; - atomic distinct: "touchdown"

;; RocketDescent -> String
;; outputs the rocket's remaining descent distance or if the rocket has landed
(check-expect (rocketDescentMessage rocketDescent1) "50 kilometers until touchdown.")
(check-expect (rocketDescentMessage rocketDescent2) "The rocket has landed!")

;; (define (rocketDescentMessage rd) "a")
;; <template from RocketDescent>

(define (rocketDescentMessage rd)
    (cond [(number? rd) 
           (string-append (number->string rd) " kilometers until touchdown.")]
          [else "The rocket has landed!"]))
