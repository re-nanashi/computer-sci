(require 2htdp/image)

;; ===========
;; Constants:

(define CELL_WIDTH 200)
(define CELL_HEIGHT 30)

(define TEXT_SIZE 20)
(define TEXT_COLOR "black")

;; ==================
;; Data definitions:

(define-struct player (name team))
;; Player is (make-player String Natural[1,2])
;; interp. a dodgeball player.
;;      (make-player s t) represents the player named s
;;      who plays on team t
(define P0 (make-player "Samael" 1))
(define P1 (make-player "Georgia" 2))
(define P2 (make-player "Joey" 1))
(define P3 (make-player "Peter" 2))
(define P4 (make-player "Thomas" 1))
(define P5 (make-player "Arthur" 2))

#;
(define (fn-for-player p)
  (... (player-name p)
       (player-team p)))

;; ListOfPlayer is one of:
;; - empty
;; - (cons Player ListOfPlayer)
;; interp. list of players.
(define LOP0 empty)
(define LOP1 (cons P0 (cons P1 (cons P2 (cons P3 (cons P4 (cons P5 empty)))))))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
          (... (first lop)
               (fn-for-lop (rest lop)))]))

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. list of String.
(define LOS0 empty)
(define LOS1 (cons "Samael" (cons "Georgia" empty)))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
          (... (first los)
               (fn-for-los (rest los)))]))

;; ==================
;; Functions:

;; selectPlayers( ListOfPlayer Natural[1,2] ) -> ListOfStrings
;; given a list and a team, produce a list of players for that team 
;; !!! make it list of strings 
(check-expect (selectPlayers LOP0 1) empty)
(check-expect (selectPlayers LOP1 1) (cons "Samael" (cons "Joey" (cons "Thomas" empty))))
(check-expect (selectPlayers LOP1 2) (cons "Georgia" (cons "Peter" (cons "Arthur" empty))))

;(define (selectPlayers lop team) lop)       ;stub

(define (selectPlayers lop t)
  (cond [(empty? lop) empty]
        [else
          (if (checkIfPartofTeam (first lop) t) 
            (cons (player-name (first lop)) (selectPlayers (rest lop) t))
            (selectPlayers (rest lop) t))]))

;; checkIfPartofTeam( Player Natural[1,2] ) -> Boolean 
;; produce true if player is part of the given team
(check-expect (checkIfPartofTeam P0 2) false)
(check-expect (checkIfPartofTeam P1 2) true)

;(define (checkIfPartofTeam p t) false) ;stub

(define (checkIfPartofTeam p t)
  (equal? (player-team p) t))

;; renderRoster( ListOfPlayer ) -> Image
;; render a game roster from the given list of players
(check-expect (renderRoster empty)
              (beside/align "top"
                            (overlay (text "Team 1" TEXT_SIZE TEXT_COLOR) 
                                     (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                            (overlay (text "Team 2" TEXT_SIZE TEXT_COLOR) 
                                     (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))))

(check-expect (renderRoster LOP1)
              (beside/align "top"
                            (above 
                              (overlay (text "Team 1" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                              (overlay (text "Samael" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                              (overlay (text "Joey" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                              (overlay (text "Thomas" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR)))
                            (above 
                              (overlay (text "Team 2" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                              (overlay (text "Georgia" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                              (overlay (text "Peter" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                              (overlay (text "Arthur" TEXT_SIZE TEXT_COLOR) 
                                       (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR)))))

; (define (renderRoster lop) empty-image) ;stub

(define (renderRoster lop)
  (beside/align "top" 
                (renderTeam (selectPlayers lop 1) 1) 
                (renderTeam (selectPlayers lop 2) 2)))

;; renderTeam( ListOfStrings Natural ) -> Image
;; render a team as a column of cell

(check-expect (renderTeam empty 1) empty-image)
(check-expect (renderTeam (cons "Samael" empty) 1) 
              (above
                (overlay
                  (text "Team 1" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                (overlay
                  (text "Samael" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                empty-image))

(check-expect (renderTeam (cons "Samael" (cons "Georgia" empty)) 2) 
              (above
                (overlay
                  (text "Team 2" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                (overlay
                  (text "Samael" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                (overlay
                  (text "Georgia" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))))

;(define (renderTeam los t) empty-image) ; stub
; <template from ListOfString>

(define (renderTeam los t)
  (above (renderCell (string-append "Team " (number->string t)))
         (renderNames los)))

;; renderNames -> Image
;; render a list of Names(strings) as a column of cells
(check-expect (renderNames empty) empty-image)
(check-expect (renderNames (cons "Samael" empty)) 
              (above
                (overlay
                  (text "Samael" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                empty-image))

(check-expect (renderNames (cons "Samael" (cons "Georgia" empty))) 
              (above
                (overlay
                  (text "Samael" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))
                (overlay
                  (text "Georgia" TEXT_SIZE TEXT_COLOR)
                  (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR))))

;(define (renderNames los) empty-image) ; stub
; <template from ListOfString>
(define (renderNames los)
  (cond [(empty? los) empty-image]
        [else
          (above (renderCell (first los))
                 (renderNames (rest los)))]))

;; String -> Image
;; render a cell of the game table given a string
(check-expect (renderCell "Team 1")
              (overlay
                (text "Team 1" TEXT_SIZE TEXT_COLOR)
                (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR)))

;(define (render-cell s) empty-image) ; stub
; <template from String>

(define (renderCell s)
  (overlay 
    (text s TEXT_SIZE TEXT_COLOR)
    (rectangle CELL_WIDTH CELL_HEIGHT "outline" TEXT_COLOR)))
