;; BikeRoute is one of:
;; - "Separated Bikeway"
;; - "Local Street Bikeway"
;; - "Painted Bike Lane"
;; - "Painted Shared-Use Lane"
;; interp. varieties of designated bike routes
;; <examples are redundant for enumerations>

#;
(define (fn-for-bike-route br)
  (cond [(string=? "Separated Bikeway" br) (...)]
        [(string=? "Local Street Bikeway" br) (...)]
        [(string=? "Painted Bike Lane" br) (...)]
        [(string=? "Painted Shared-Use Lane" br) (...)]))

;; Template rules used:
;; - one of: 3 cases
;; - atomic distinct: "Separated Bikeway"
;; - atomic distinct: "Local Street Bikeway"
;; - atomic distinct: "Painted Bike Lane"
;; - atomic distinct: "Painted Shared-Use Lane"

;; BikeRoute -> Boolean
;; produces true if the given BikeRoute is exclusively designated for bicycles
(check-expect (exclusive? "Separated Bikeway") true)
(check-expect (exclusive? "Local Street Bikeway") false)
(check-expect (exclusive? "Painted Bike Lane") true)
(check-expect (exclusive? "Painted Shared-Use Lane") false)

; (define (exclusive? br) false)     ;stub
;; <Template from BikeRoute>

(define (exclusive? br)
  (cond [(or (string=? "Separated Bikeway" br) 
             (string=? "Painted Bike Lane" br)) true]
        [(or (string=? "Local Street Bikeway" br) 
             (string=? "Painted Shared-Use Lane" br)) false]))
