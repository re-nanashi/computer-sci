;; Data definitions:

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))
(define H2 (shared ((-0- (make-room "A" (list (make-room "B" (list -0-)))))) -0-))
(define H3 (shared ((-A- (make-room "A" (list -B-)))
                    (-B- (make-room "B" (list -C-)))
                    (-C- (make-room "C" (list -A-))))
                   -A-))
(define H4 (shared ((-A- (make-room "A" (list -B- -D-)))
                    (-B- (make-room "B" (list -C- -E-)))
                    (-C- (make-room "C" (list -B-)))
                    (-D- (make-room "D" (list -E-)))
                    (-E- (make-room "E" (list -A- -F-)))
                    (-F- (make-room "F" empty)))
                   -A-))

;; <template: structural recursion, encapsulate w/ local, tail-recursion w/ worklist acc
;;            context-preserving acc on what rooms we have already visited.
#;
(define (fn-for-house r0)
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
              (fn-for-lor todo visited)
              (fn-for-lor (append (room-exits r) todo) 
                          (cons (room-name r) visited)))) 
          (define (fn-for-lor todo visited) 
            (cond [(empty? todo) (...)]
                  [else
                    (fn-for-room (first todo) 
                                 (rest todo) 
                                 visited)]))]
    (fn-for-room r0 empty empty)))

;; Room String -> Boolean
;; produce true if it's possible to reach a room from given room
(check-expect (reachable? H1 "A") true)
(check-expect (reachable? H1 "B") true)
(check-expect (reachable? H1 "C") false)
(check-expect (reachable? (first (room-exits H1)) "A") false)
(check-expect (reachable? H4 "F") true)

(define (reachable? r0 n)
  (local [(define (fn-for-room r todo visited) 
            (cond [(string=? (room-name r) n) true]
                  [(member (room-name r) visited) 
                   (fn-for-lor todo visited)]
                  [else
                    (fn-for-lor (append (room-exits r) todo) 
                            (cons (room-name r) visited))])) 
          (define (fn-for-lor todo visited) 
            (cond [(empty? todo) false]
                  [else
                    (fn-for-room (first todo) 
                                 (rest todo) 
                                 visited)]))]
    (fn-for-room r0 empty empty)))

