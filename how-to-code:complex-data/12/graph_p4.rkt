;; Data Definitions: 

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

(define H1 (make-room "A" (list (make-room "B" empty))))
(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 
(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))
(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

;; template; structural recursion, encapsulate w/ local, tail-recursive w/ worklist
;;           context-preserving accumulator what rooms traversed on this path
#;
(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context-preserving accumulator, names of rooms
  (local [(define (fn-for-room r todo visisted)
            (if (member (room-name r) visisted)
              (fn-for-lor todo visisted)
              (fn-for-lor (append (room-exits r) todo)
                          (cons (room-name r) visisted))))

          (define (fn-for-lor lor visisted)
            (cond [(empty? lor) (...)]
                  [else 
                    (fn-for-room (first lor) 
                                 (rest lor) 
                                 visisted) ]))]
    (fn-for-room r0 empty empty)))

;; Rooms -> Room
;; produce the room with the most exits. 
;; In case of a tie, produce any of the room.
(check-expect (max-exits-from H1) H1)
(check-expect (max-exits-from H2) H2)
(check-expect (max-exits-from (shared ((-A- (make-room "A" (list -B-)))
                                       (-B- (make-room "B" (list -C- -A-)))
                                       (-C- (make-room "C" (list -A-))))
                                -A-)) 
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list -C- -A-)))
                       (-C- (make-room "C" (list -A-))))
                -B-))

(define (max-exits-from r0)
  (local [(define (fn-for-room r todo visisted most_exits)
            (if (member (room-name r) visisted)
              (fn-for-lor todo visisted most_exits)
              (fn-for-lor (append (room-exits r) todo)
                          (cons (room-name r) visisted)
                          (max-exits r most_exits))))

          (define (fn-for-lor lor visisted most_exits)
            (cond [(empty? lor) most_exits]
                  [else 
                    (fn-for-room (first lor) 
                                 (rest lor) 
                                 visisted
                                 most_exits)]))
          (define (max-exits r mst_r)
            (if (> (length (room-exits r)) (length (room-exits mst_r)))
              r
              mst_r))]
    (fn-for-room r0 empty empty (make-room "" empty))))
