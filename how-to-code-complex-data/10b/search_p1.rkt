(require racket/list)

;; Maze Solver

;; In a maze, the board is 5x5 grid of SQUARES.
;; There are edges, walls, and a goal.

;; The idea of the game is to start the maze from upper left
;; then traverse the maze to get to the goal.

;; =================
;; Data Definitions:

;; Val is Number[0, 1]

;; Maze is (list of Val|Boolean) that is 25 elements long
;; interp.
;; Visually a maze is a 5x5 array of squares, where each square
;; has an index. But we represent it as a single flat list. 
;; C represents current position or 1 if seen. 
;; B represents block or wall.
;; G represents goal.

;; Pos is Natural[0, 24]
;; interp.
;;  the position of a square on the board


;; =================
;; Constants:

(define RIGHT_EDGE (list 4 9 14 19))
(define BOTTOM_EDGE (list 20 21 22 23))

(define C 1)
(define G true)
(define B false)

(define MZ1
  (list C B B B B
        0 0 B 0 0
        B 0 B B B
        0 0 B B B
        0 0 0 0 G))

(define MZ2
  (list C 0 0 0 0
        0 B B B B
        0 B B B B
        0 B B B B
        0 0 0 0 G))

(define MZ3
  (list C 0 0 0 0
        0 B B B 0
        0 B B B 0
        0 B B B 0
        0 B B B G))

(define MZ4
  (list C 0 0 0 0
        0 B B B 0
        0 B 0 0 0
        0 B 0 B B
        B B 0 0 G))

(define MZ5
  (list C B 0 0 0
        B B B B 0
        0 B 0 0 0
        0 B 0 B B
        B B 0 0 G))

;; ============
;; Functions:

;; Maze -> Boolean
;; produce true if maze is solvable, otherwise return false

(check-expect (check_maze MZ1) true)
(check-expect (check_maze MZ2) true)
(check-expect (check_maze MZ3) true)
(check-expect (check_maze MZ4) false)
(check-expect (check_maze MZ5) false)

;(define (check_maze mz) false) ; stub

(define (check_maze mz)
  (local [(define (solve__mz mz)
            (cond [(goal_is_reached mz) true]
                  [else
                    (solve__lomz (next_mzs mz))]))

          (define (solve__lomz lomz)
            (cond [(empty? lomz) false]
                  [else
                    (local [(define try (solve__mz (first lomz)))]
                      (if (not (false? try))
                        try
                        (solve__lomz (rest lomz))))]))] 
    (solve__mz mz)))


;; Maze -> Boolean
;; produce true if C's next position is the goal
;; ASSUME: Maze is valid, there is only 1 instance of C
(check-expect (goal_is_reached MZ1) false)
(check-expect (goal_is_reached MZ4) false)
(check-expect (goal_is_reached (swap_pos MZ1 0 19)) true)
(check-expect (goal_is_reached (swap_pos MZ1 0 23)) true)

;(define (goal_is_reached mz) false) ; stub

(define (goal_is_reached mz) 
  (local [(define current_pos (index-of mz C))]
    (or (= current_pos 19) (= current_pos 23))))

;; Maze -> (listof Maze)
;; produce valid, next maze 
(check-expect (next_mzs MZ1) (list (swap_pos MZ1 0 5)))
(check-expect (next_mzs MZ3)
              (list (swap_pos MZ3 0 1)
                    (swap_pos MZ3 0 5)))

;(define (next_mzs mz) empty)    ; stub

(define (next_mzs mz)
 (generate_next (find_next_steps mz) mz))

;; Maze -> (listof Pos) 
;; produces a list of positions that C can travel to.
(check-expect (find_next_steps MZ1) (list 5))
(check-expect (find_next_steps MZ2) (list 1 5))
(check-expect (find_next_steps (swap_pos MZ3 0 9)) (list 14))

(define (find_next_steps mz)
  (local [(define current_pos (index-of mz C))
          (define next_step_list 
            (local [(define (check_if_current_member lst) 
                      (not (false? (member current_pos lst))))]
              (cond [(check_if_current_member RIGHT_EDGE) 
                     (list (+ current_pos 5))]
                    [(check_if_current_member BOTTOM_EDGE) 
                     (list (+ current_pos 1))]
                    [else
                      (list (+ current_pos 1) (+ current_pos 5))])))
          (define (valid p) (not (false? (read_pos mz p))))]
    (filter valid next_step_list)))

;; (listof Pos) Maze -> (listof Maze)
;; produce new mazes according to list of valid positions by swapping the old C to new C
(check-expect (generate_next (list 5) MZ1) (list (swap_pos MZ1 0 5)))
(check-expect (generate_next (list 1 5) MZ3) (list (swap_pos MZ3 0 1)
                                                   (swap_pos MZ3 0 5)))

(define (generate_next lop mz)
  (map (lambda (p) (swap_pos mz (index-of mz C) p)) lop))

;; Maze Pos -> Val or false
;; produce the value at the given position on the given board.
(check-expect (read_pos MZ1 0) C)
(check-expect (read_pos MZ1 24) true)
(check-expect (read_pos MZ2 9) B)

(define (read_pos mz p)
  (list-ref mz p))

;; Maze Pos Pos -> Maze
;; produce new board by swapping old position and new position 
(check-expect (swap_pos MZ1 0 1) (cons B (cons C (rest (rest MZ1)))))
(check-expect (swap_pos (list C 0 0 0 0 
                              0 B B B 0 
                              0 B B B 0 
                              0 B B B 0 
                              0 B B B G) 
                        0 24)
              (list G 0 0 0 0 
                    0 B B B 0 
                    0 B B B 0 
                    0 B B B 0 
                    0 B B B C))

(define (swap_pos mz op np)
  (local [(define (fill_square b p nv)
            (append (take b p)
                    (list nv)
                    (drop b (add1 p))))
          (define (swap op np) 
            (fill_square 
              (fill_square mz op (read_pos mz np)) 
              np (read_pos mz op)))]
    (swap op np)))
