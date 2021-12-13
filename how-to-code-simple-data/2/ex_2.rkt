;; BuildingStatus is one of:
;; - "new"
;; - "old"
;; - "heritage"
;; interp. classification levels of how old buildings are in Vancouver

;; <examples are redundant for enumerations>

#;
(define (buildingStatusFunc bs)
  (cond [(string=? "new" bs) (...)]
        [(string=? "old" bs) (...)]
        [(string=? "heritage" bs) (...)]))

;; Templates rules used:
;; - one of: 3 cases
;; - atomic distinct: "new"
;; - atomic distinct: "old"
;; - atomic distinct: "heritage"

;; BuildingStatus -> Boolean
;; produces true if the given BuildingStatus is classified as "old"
(check-expect (checkIfToBeDemolished "new") false)
(check-expect (checkIfToBeDemolished "old") true)
(check-expect (checkIfToBeDemolished "heritage") false)

;; (define (checkIfToBeDemolished bs) false)    ;stub
;; <template from BuildingStatus>

(define (checkIfToBeDemolished bs)
  (string=? bs "old"))
