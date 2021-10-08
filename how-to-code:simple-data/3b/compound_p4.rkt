(define-struct trip (origin destination modeOfTransport duration))
;; Trip is (make-trip String String String Natural)
;; interp. a trip with origin, destination, mode of transport and duration

(define trip1 (make-trip "Vancouver" "Cancun" "Flight" 10))
(define trip2 (make-trip "Calgary" "Ottawa" "Car" 14))
(define trip3 (make-trip "Montreal" "New York" "Flight" 5))

#;
(define (tripFunc t)
  (... (trip-origin)                ; String
       (trip-destination)           ; String 
       (trip-modeOfTransport)       ; String
       (trip-duration)))            ; Natural

;; Template rules used:
;; - compound: 4 fields

;; Trip Trip -> Trip
;; prodcue the trip with the longer duration, if durations are equal return second trip
(check-expect (longerTrip trip2 trip3) trip2)
(check-expect (longerTrip trip3 trip1) trip1)
(check-expect (longerTrip trip3 (make-trip "Houston" "Dallas" "Car" 5)) (make-trip "Houston" "Dallas" "Car" 5))

;(define (longerTrip trip1 trip2) Trip)     ;stub

(define (longerTrip t1 t2)
  (if (> (trip-duration t1) (trip-duration t2))
    t1
    t2))
