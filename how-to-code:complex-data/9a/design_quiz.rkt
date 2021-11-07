;; Player is String
;; interp. the name of a tennis player.
(define P0 "Maria")
(define P1 "Serana")

#;
(define (fn-for-player p)
  (... p))

;; Roster is one of:
;; - empty
;; - (cons Player Roster)
;; interp. a team roster, ordered from best player to worst.
(define R0 empty)
(define R1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define R2 (list "Maria" "Nadia" "Elena" "Svetlana"))
(define R3 (list "John" "Maria" "Nadia" "Elena" "Svetlana"))

#;
(define (fn-for-roster r)
  (cond [(empty? r) (...)]
        [else
          (... (fn-for-player (first r))
               (fn-for-roster (rest r)))]))

(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; interp. a match between player p1 and player p2, with same team rank
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

#;
(define (fn-for-match m)
  (... (match-p1 m)
       (match-p2 m)))

;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; interp. a list of matches between one team and another.
(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria")
                   (make-match "Gabriela" "Nadia")))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
          (... (fn-for-match (first lom))
               (fn-for-lom (rest lom)))]))

;; Roster Roster -> Boolean
;; produce true if all players on both teams will play 
;; roster length for both teams are equal
(check-expect (allPlayersWillPlay empty empty) true)
(check-expect (allPlayersWillPlay (list "John") empty) false)
(check-expect (allPlayersWillPlay empty (list "John")) false)
(check-expect (allPlayersWillPlay R1 R2) true)
(check-expect (allPlayersWillPlay R1 R3) false)

(define (allPlayersWillPlay r1 r2)
  (cond [(and (empty? r1) (empty? r2)) true]
        [(or (empty? r1) (empty? r2)) false]
        [else
          (allPlayersWillPlay (rest r1) (rest r2))]))

;; Roster Roster -> ListOfMatch or false
;; produce list of matches that would be played if both teams have equal number of players.
;; otherwise return false
(check-expect (generateMatches empty empty) empty)
(check-expect (generateMatches (list "John") empty) false)
(check-expect (generateMatches empty (list "John")) false)
(check-expect (generateMatches R1 R3) false)
(check-expect (generateMatches R1 R2) (list (make-match "Eugenie" "Maria")
                                               (make-match "Gabriela" "Nadia")
                                               (make-match "Sharon" "Elena")
                                               (make-match "Aleksandra" "Svetlana")))

(define (generateMatches r1 r2)
  (local [(define (allPlayersWillPlay r1 r2) 
            (cond [(and (empty? r1) (empty? r2)) true] 
                  [(or (empty? r1) (empty? r2)) false] 
                  [else 
                    (allPlayersWillPlay (rest r1) (rest r2))])) 
          (define (generateListOfMatches r1 r2) 
            (cond [(empty? r1) empty] 
                  [else 
                    (cons (make-match (first r1) (first r2)) 
                          (generateListOfMatches (rest r1) (rest r2)))]))] 
    (if (allPlayersWillPlay r1 r2) 
      (generateListOfMatches r1 r2) 
      false)))
