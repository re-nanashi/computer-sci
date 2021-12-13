;; A simple solution to the generic nqueens problem.
;; - uses lambda to reduce and simplify the code

;; Data definitions:

;; Position is Natural
;; interp. positions on the board
;;          if N is the number of queens
;;          then (sqr N) is the number of positions on the board
;;          so this number should be in [0, (- (sqr N) 1)]
(define P1 0)
(define P2 (- 16 1))

;; Board is (listof Position) up to N elements long
;; interp. the positions of the queens that have been placed on the board
(define BD1 empty)
(define BD2 (list 0))
(define BD3 (list 14 8 7 1))    ;a solution to 4x4 puzzle


;; Functions:

;; Natural -> Board or false 
;; produce first found solution for n queens of size N; or false if none exists.
(check-expect (nqueens 1) (list 0))
(check-expect (nqueens 2) false)
(check-expect (nqueens 3) false)
(check-expect (nqueens 4) (list 14 8 7 1))
(check-expect (nqueens 5) (list 23 16 14 7 0))
(check-expect (nqueens 6) (list 34 26 18 17 9 1))
(check-expect (nqueens 7) (list 47 38 29 27 18 9 0))
(check-expect (nqueens 8) (list 59 49 46 34 29 23 12 0))

(define (nqueens N)
  (local [;; Board -> Board or false
          ;; do backtracking search of generated arb-arity tree of boards
          (define (fn-for-bd bd)
            (if (solved? bd)
              bd
              (fn-for-lobd (next-bds bd))))

          (define (fn-for-lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                    (local [(define try (fn-for-bd (first lobd)))] 
                      (if (not (false? try)) 
                        try 
                        (fn-for-lobd (rest lobd))))]))

          ;; Board -> Boolean
          ;; produce true if board has N queens.
          (define (solved? bd) (= (length bd) N))

          ;; Board -> (listof Board)
          ;; produce next valid board
          (define (next-bds bd)
            (map (lambda (p) (cons p bd))
                 (filter (lambda (p) 
                           (no_attack p bd))
                         (build-list (sqr N) identity))))

          (define (no_attack p bd)
            (andmap (lambda (q)
                      (not (attack? q p)))
                    bd))

          ;; Position Position -> Boolean
          ;; produce true if queens at position a and b attack each other
          (define (attack? pa pb)
            (local [(define x1 (pos-x pa))
                    (define y1 (pos-y pa))
                    (define x2 (pos-x pb))
                    (define y2 (pos-y pb))]
              (or (= x1 x2)
                  (= y1 y2)
                  (= (/ (- y2 y1) (- x2 x1)) 1)
                  (= (/ (- y2 y1) (- x2 x1)) -1))))

          ;; Pos -> Natural[0, N]
          ;; produce the row or column for the given position
          (define (pos-x p) (remainder p N))
          (define (pos-y p) (quotient p N))]
    (fn-for-bd empty)))
