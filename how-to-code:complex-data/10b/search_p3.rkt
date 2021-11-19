(require racket/list)

;; Brute force N-queens solver

;; =================
;; Data definitions:

;; Val is one of:
;; - false
;; - 1

;; Board is (listof Val) that is n^2 elements long
;; interp. 
;; Visually a board is n x n array of squares, where each square 
;; has a row and column number (r, c). But we represent it as a 
;; single flat list.

;; Pos is Natural[0, (- (n^2) 1)]
;; interp.
;;      the position of a square on the board, for a given p, then
;;      - the row is (quotient p n)
;;      - the column is (remainder p n)

;; Convert 0-based column and row to Pos
(define (convert_to_pos n c r) (+ (* r n) c))

;; Convert Pos to Posn
(define (convert_to_r_c n p)
  (make-posn (remainder p n)
             (quotient p n)))

;; Unit is (listof Pos) of length n
;; interp.
;;  The position of every square in a unit. There are n x 2
;; of these for the n rows, n columns.

;; =================
;; Constants:

(define Q 1)

(define B false)    ; B stands for blank

;; !!! Create a function that generates a list of n^2
;; !!! 
