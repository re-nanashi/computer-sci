;; Definitions

;; CityName is a string
;; interp. the name of the city
(define cityName1 "Manila")
(define cityName2 "Batangas")
#;
(define (cityNameFunc cn)
  (...cn))

;; Template rules used:
;; - atomic non-distinct: string

;; Functions

;; CityName -> Boolean
;; produces true if given city is Manila
(check-expect (checkIfBestCity cityName1) true)
(check-expect (checkIfBestCity cityName2) false)

;; (define (checkIfBestCity cityName) false)    ;stub

#;
(define (checkIfBestCity? cityName)
  (...cityName))

(define (checkIfBestCity? cityName)
  (string=? cityName "Manila"))
