(require racket/list)

;; Brute force Sudoku solver

;; In Sudoku, the board is a 9x9 grid of SQUARES. 
;; There are 9 ROWS and COLUMNS, there are also 9
;; 3x3 BOXES. Rows, columns and boxes are all UNITS.
;; So there are 27 units.

;; The ides of the game is to fill each square with 
;; a Natural[1, 9] such that no unit contains a duplicate
;; number.

;; =================
;; Data definitions:

;; Val is Natural[1, 9]

;; Board is (list of Val|false) that is 81 elements lons
;; interp.
;; Visually a board is 9x9 array of squares, where each square
;; has a row and column number (r, c). But we represent it as a
;; single flat list, in which the rows are layed out one after another
;; in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; Pos is Natural[0, 80]
;; interp. 
;;  the position of a square on the board, for a given p, then
;;      - the row is (quotient p 9)
;;      - the column is (remainder p 9)

;; Convert 0-based row and column to Pos
(define (convert_to_pos r c) (+ (* r 9) c))     ;helpful for writing tests

;; Unit is (listof Pos) of legnth 9
;; interp. 
;;  The position of every square in a unit. There are
;; 27 of these for the 9 rows, 9 columns and 9 boxes.


;; =================
;; Constants:

(define ALL_VALS (list 1 2 3 4 5 6 7 8 9))

(define B false) ; B stands for blank

(define BD1 
  (list B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD2 
  (list 1 2 3 4 5 6 7 8 9 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B 
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B
        B B B B B B B B B))

(define BD3 
  (list 1 B B B B B B B B
        2 B B B B B B B B
        3 B B B B B B B B
        4 B B B B B B B B
        5 B B B B B B B B
        6 B B B B B B B B
        7 B B B B B B B B
        8 B B B B B B B B
        9 B B B B B B B B))

(define BD4                ;easy
  (list 2 7 4 B 9 1 B B 5
        1 B B 5 B B B 9 B
        6 B B B B 3 2 8 B
        B B 1 9 B B B B 8
        B B 5 1 B B 6 B B
        7 B B B 8 B B B 3
        4 B 2 B B B B B 9
        B B B B B B B 7 B
        8 B B 3 4 9 B B B))

(define BD4s               ;solution to 4
  (list 2 7 4 8 9 1 3 6 5
        1 3 8 5 2 6 4 9 7
        6 5 9 4 7 3 2 8 1
        3 2 1 9 6 4 7 5 8
        9 8 5 1 3 7 6 4 2
        7 4 6 2 8 5 9 1 3
        4 6 2 7 5 8 1 3 9
        5 9 3 6 1 2 8 7 4
        8 1 7 3 4 9 5 2 6))

(define BD5                ;hard
  (list 5 B B B B 4 B 7 B
        B 1 B B 5 B 6 B B
        B B 4 9 B B B B B
        B 9 B B B 7 5 B B
        1 8 B 2 B B B B B 
        B B B B B 6 B B B 
        B B 3 B B B B B 8
        B 6 B B 8 B B B 9
        B B 8 B 7 B B 3 1))

(define BD5s               ;solution to 5
  (list 5 3 9 1 6 4 8 7 2
        8 1 2 7 5 3 6 9 4
        6 7 4 9 2 8 3 1 5
        2 9 6 4 1 7 5 8 3
        1 8 7 2 3 5 9 4 6
        3 4 5 8 9 6 1 2 7
        9 2 3 5 4 1 7 6 8
        7 6 1 3 8 2 4 5 9
        4 5 8 6 7 9 2 3 1))

(define BD6                ;hardest ever? (Dr Arto Inkala)
  (list B B 5 3 B B B B B 
        8 B B B B B B 2 B
        B 7 B B 1 B 5 B B 
        4 B B B B 5 3 B B
        B 1 B B 7 B B B 6
        B B 3 2 B B B 8 B
        B 6 B 5 B B B B 9
        B B 4 B B B B 3 B
        B B B B B 9 7 B B))

(define BD7                 ; no solution 
  (list 1 2 3 4 5 6 7 8 B 
        B B B B B B B B 2 
        B B B B B B B B 3 
        B B B B B B B B 4 
        B B B B B B B B 5
        B B B B B B B B 6
        B B B B B B B B 7
        B B B B B B B B 8
        B B B B B B B B 9))




;; Positions of all the rows, columns and boxes:

(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))

(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXES
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

(define UNITS (append ROWS COLS BOXES))

;; =================
;; Functions:

;; Board -> Board or false
;; produce a solution for bd; or false if bd is unsolvable
;; Generative -> Arbitray-arity tree -> Backtracking search
;; Assume: bd is valid
;; !!!
(check-expect (solve BD4) BD4s)
(check-expect (solve BD5) BD5s)
(check-expect (solve BD7) false)

;(define (solve bd) false)   ;stub

(define (solve bd)
  (local [(define (solve__bd bd)
            (cond [(is_solved bd) bd]
                  [else 
                    (solve__lobd (next_bds bd))])) 

          (define (solve__lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                    (local [(define try (solve__bd (first lobd)))]
                     (if (not (false? try))
                       try 
                       (solve__lobd (rest lobd))))]))]
    (solve__bd bd)))

;; Board -> Boolean
;; produce true if Board is solved.
;; ASSUME: board is valid, so it is solved if it is full.
(check-expect (is_solved BD1) false)
(check-expect (is_solved BD2) false)
(check-expect (is_solved BD4s) true)

;(define (is_solved bd) false)   ; stub

(define (is_solved bd)
  (andmap (lambda (n) (not (false? n))) bd))

;; Board -> (listof Board)
;; produce valid next boards from board.
(check-expect (next_bds (cons 1 (rest BD1)))
              (list (cons 1 (cons 2 (rest (rest BD1))))
                    (cons 1 (cons 3 (rest (rest BD1))))
                    (cons 1 (cons 4 (rest (rest BD1))))
                    (cons 1 (cons 5 (rest (rest BD1))))
                    (cons 1 (cons 6 (rest (rest BD1))))
                    (cons 1 (cons 7 (rest (rest BD1))))
                    (cons 1 (cons 8 (rest (rest BD1))))
                    (cons 1 (cons 9 (rest (rest BD1))))))

;(define (next_bds bd) empty)    ; stub
(define (next_bds bd)
  (keep_only_valid (fill_blank (find_blank bd) bd)))

;; Board -> Pos
;; produces the position of the first blank square
;; ASSUME: the board has atleast one blank square
(check-expect (find_blank BD1) 0)
(check-expect (find_blank BD3) 1)

;(define (find_blank bd) 0)      ; stub

(define (find_blank bd) 
  (cond [(empty? bd) (error "Error has occured.")]
        (else
          (if (false? (first bd))
            0
            (+ 1 (find_blank (rest bd)))))))


;; Pos Board -> (listof Board)
;; produce 9 boards, with blank filled with Natural[1, 9]
(check-expect (fill_blank 0 BD1)
              (list (cons 1 (rest BD1))
                    (cons 2 (rest BD1))
                    (cons 3 (rest BD1))
                    (cons 4 (rest BD1))
                    (cons 5 (rest BD1))
                    (cons 6 (rest BD1))
                    (cons 7 (rest BD1))
                    (cons 8 (rest BD1))
                    (cons 9 (rest BD1))))

;(define (fill_blank p bd) empty)     ; stub

(define (fill_blank p bd)
  (local [(define (fill n)
            (fill_square bd p n))]
   (map fill ALL_VALS)))

;(listof Board) -> (listof Board)
;; produce list containing only valid boards
(check-expect (keep_only_valid (list (cons 1 (cons 1 (rest (rest BD1)))))) empty)
(check-expect (keep_only_valid (list BD1 (cons 1 (cons 1 (rest (rest BD1)))))) (list BD1))

;(define (keep_only_valid lob) empty)    ; stub

(define (keep_only_valid lob)
  (filter check_if_valid lob))

;; Board -> Boolean
;; produce true if no unit on the board has the same value twice.
(check-expect (check_if_valid BD1) true)
(check-expect (check_if_valid BD2) true)
(check-expect (check_if_valid BD3) true)
(check-expect (check_if_valid BD4) true)
(check-expect (check_if_valid BD5) true)
(check-expect (check_if_valid (cons 2 (rest BD2))) false)
(check-expect (check_if_valid (cons 2 (rest BD3))) false)
(check-expect (check_if_valid (fill_square BD4 1 6)) false)

(define (check_if_valid bd)
  (local [(define (handle_check n)
            (check_if_unit_is_twice n bd))]
    (andmap handle_check (build-list 81 identity))))

;; Natural[0 80] Board -> Boolean
;; produce true if the value at the current position does not have the same value inside it's list.
(check-expect (check_if_unit_is_twice 0 (cons 2 (rest BD2))) false)

(check-expect (check_if_unit_is_twice 0 BD2) true)

(check-expect (check_if_unit_is_twice 0 BD4s) true)
(check-expect (check_if_unit_is_twice 80 BD5s) true)
(check-expect (check_if_unit_is_twice 80 BD4s) true)
(check-expect (check_if_unit_is_twice 0 (cons 2 (rest BD3))) false)

(define (check_if_unit_is_twice pos bd)
  (and (check_row pos bd)
       (check_col pos bd)
       (check_box pos bd)))

;; Pos Board -> Boolean
;; produce true if Pos' value does not have the same value on the same row.
(check-expect (check_col 0 BD4s) true)
(check-expect (check_box 0 BD4s) true)
(check-expect (check_row 0 BD4s) true)
(check-expect (check_col 0 BD4) true)
(check-expect (check_col 0 (cons 1 (rest BD4s))) false)

(define (check_row pos bd)
  (abstract_check pos bd ROWS))

(define (check_col pos bd)
  (abstract_check pos bd COLS))

(define (check_box pos bd)
  (abstract_check pos bd BOXES))

(define (abstract_check pos bd unit)
  (check_if_twice (read_square bd pos)
                  (convert_to_values (get_list pos unit) bd)))

;; Val (listof Val) -> Boolean
;; produce true if there are no 2 instances of Val in (listof Val)
(check-expect (check_if_twice 2 (list 2 1 6 3 9 7 4 5 8)) true)
(check-expect (check_if_twice 1 (list 2 1 6 3 9 7 4 5 8)) true)
(check-expect (check_if_twice 2 (list 2 2 6 3 9 7 4 5 8)) false)
(check-expect (check_if_twice 6 (list 2 6 6 3 9 7 4 5 8)) false)
(check-expect (check_if_twice false (list 2 false 6 3 9 7 4 5 8)) true)

(define (check_if_twice n lon)
  (local [(define (pred v) 
            (equal? v n))]
    (if (false? n)
      true
      (< (count pred lon) 2))))

;; (listof Pos) -> (listof Val)
;; Convert the a list of positions that was converted to it's values 
(check-expect (convert_to_values (list 0 9 18 27 36 45 54 63 72) BD4s) (list 2 1 6 3 9 7 4 5 8))
(check-expect (convert_to_values (list 0 9 18 27 36 45 54 63 72) BD4) (list 2 1 6 B B 7 4 B 8))

(define (convert_to_values lst bd)
  (local [(define (fn pos)
            (read_square bd pos))]
    (map fn lst)))

;; Pos (listof (listof Pos)) -> (listof Pos)
;; produce the list where Pos is an item of.
(check-expect (get_list 27 COLS) (list 0  9 18 27 36 45 54 63 72))
(check-expect (get_list 27 ROWS) (list 27 28 29 30 31 32 33 34 35))
(check-expect (get_list 27 BOXES) (list 27 28 29 36 37 38 45 46 47))

(define (get_list pos lst)
  (cond [(empty? lst) (error "Error. pos should be inside the list.")]
        [else
          (if (search pos (first lst))
            (first lst)
            (get_list pos (rest lst)))]))

;; Pos (listof Pos) -> Boolean 
;; produce true if Pos is a part of the list.
(define (search pos lst)
  (cond [(empty? lst) false]
        [else
          (local [(define mid (floor (/ (length lst) 2))) 
                  (define mid_val (list-ref lst mid))]
            (cond [(= pos mid_val) true]
                  [(< pos mid_val) (search pos (take lst mid))]
                  [(> pos mid_val) (search pos (drop lst (add1 mid)))]))]))

;; Board Pos -> Val or false
;; produce the value at given position on the given board.
(check-expect (read_square BD2 (convert_to_pos 0 5)) 6)
(check-expect (read_square BD3 (convert_to_pos 7 0)) 8)

(define (read_square bd p)
  (list-ref bd p))

;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill_square BD1 (convert_to_pos 0 0) 1)
              (cons 1 (rest BD1)))

(define (fill_square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))