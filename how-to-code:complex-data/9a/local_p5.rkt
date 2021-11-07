(define-struct bracket (team_won team_lost br_won br_lost))
;; Bracket is one of:
;; - false
;; - (make-bracket String String Bracket Bracket)
;; interp. A tournament competition bracket.
;;      false indicates an empty bracket.
;;      (make-bracket t1 t2 br1 br2) means that
;;      - team t1 beat team t2
;;      - br1 represents team t1's bracket leading to the match
;;      - br2 represents team t2's bracket leading to the match

(define B0 false) ; an empty tournament bracket

(define BE 
  (make-bracket "Riot" "Schwa" false false))

(define BF 
  (make-bracket "Nemesis" "Ozone" false false))

(define BL 
  (make-bracket "Scandal" "Capitals" BG BH)) ;; create

(define BH 
  (make-bracket "Capitals" "Traffic" false false))

(define BG 
  (make-bracket "Scandal" "Phoenix" false false))

(define BK 
  (make-bracket "Riot" "Nemesis" BE BF))

(define BN 
  (make-bracket "Scandal" "Riot" BL BK))

(define BQ
  (make-bracket "Fury" "Showdown" BJ ))

#;
(define (fn-for-bracket br)
  (cond [(false? br) (...)]
        [else
          (... (bracket-team_won br)
               (bracket-team_lost br)
               (fn-for-bracket (bracket-br_won br))
               (fn-for-bracket (bracket-br_lost br)))]))

;; ListOfTeam is one of:
;; - empty
;; - (cons String ListOfTeam)
(define T0 empty)       ; no teams
(define T2 (list "Scandal" "Traffic"))       

#; 
(define (fn-for-lot lot)
  (cond [(empty? lot) (...)]
        [else
          (... (first lot)
               (fn-for-lot (rest lot)))]))

;; Bracket ListOfTeam -> Boolean
;; produce true if the winner of the bracket has won against the teams inside the list from latest to oldest order.
(check-expect (braket_checker false empty) true)
(check-expect (braket_checker false (list "Capitals")) false)
(check-expect (braket_checker BN (list)) true)
(check-expect (braket_checker BN (list "Capitals")) true)
(check-expect (braket_checker BN (list "Riot" "Capitals" "Phoenix")) true)
(check-expect (braket_checker BN (list "Capitals" "Phoenix")) true)
(check-expect (braket_checker BN (list "Riot" "Phoenix")) true)
(check-expect (braket_checker BN (list "Phoenix" "Riot")) false)
(check-expect (braket_checker BN (list "Riot" "Ozone")) false)
(check-expect (braket_checker BN (list "Ozone" "Riot")) false)

;(define (braket_checker b lot) false)   ; stub

(define (braket_checker b lot)
  (cond [(empty? lot) true]
        [(false? b) false]
        [else
          (if (string=? (bracket-br_won b) (first lot))
            (braket_checker (bracket-br_won b) (rest lot))
            (braket_checker (bracket-br_won b) lot))]))

;; String Bracket -> String or false
;; produce the team that knocked out (i.e ousted) t in bracket br
;; or false if there isn't one.
(check-expect (ouster "Nemesis" false) false)
(check-expect (ouster "Scandal" BN) false)
(check-expect (ouster "Nemesis" BN) "Riot")
(check-expect (ouster "Traffic" BN) "Capitals")

(define (ouster t br)
  (cond [(false? br) false]
        [else
          (if (string=? t (bracket-team_lost br))
            (bracket-team_won br)
            (local [(define ouster_won ((ouster t (bracket-team_won br))))]
              (if (not (false? ouster_won)) 
                ouster_won 
                (ouster t (bracket-team_lost br)))))]))
