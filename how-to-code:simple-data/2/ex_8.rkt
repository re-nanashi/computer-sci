;; =================
;; Data definitions:

;; Direction is one of:
;;  - "N"
;;  - "S"
;;  - "E"
;;  - "W"
;; interp. a compass direction that a player can be facing
;; <examples are redundant for enumerations>
#;
(define (fn-for-direction d)
  (cond [(string=? d "N") (...)]
        [(string=? d "S") (...)]
        [(string=? d "E") (...)]
        [(string=? d "W") (...)]))

;; Template rules used:
;; - one of: 4 cases
;; - atomic distinct: "N"
;; - atomic distinct: "S"
;; - atomic distinct: "E"
;; - atomic distinct: "W"


;; =================
;; Functions:

;; Direction -> Direction
;; given a direction, produce its 90 degree left turn direction
(check-expect (leftDirection "N") "W")
(check-expect (leftDirection "W") "S")
(check-expect (leftDirection "S") "E")
(check-expect (leftDirection "E") "N")

;(define (leftDirection d) "N")     ;stub
;; <Template from Direction>

(define (leftDirection d)
  (cond [(string=? d "N") "W"]
        [(string=? d "S") "E"]
        [(string=? d "E") "N"]
        [(string=? d "W") "S"]))

