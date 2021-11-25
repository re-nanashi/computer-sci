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

;; Rooms -> Natural
;; produce the number of rooms, starting from the given room
(check-expect (count_rooms H1) 2)
(check-expect (count_rooms H2) 2)
(check-expect (count_rooms H3) 3)
(check-expect (count_rooms H4) 6)

(define (count_rooms r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context-preserving accumulator, names of rooms
  (local [(define (fn-for-room r todo visisted cnt)
            (if (member (room-name r) visisted)
              (fn-for-lor todo visisted cnt)
              (fn-for-lor (append (room-exits r) todo)
                          (cons (room-name r) visisted)
                          (add1 cnt))))

          (define (fn-for-lor lor visisted cnt)
            (cond [(empty? lor) cnt]
                  [else 
                    (fn-for-room (first lor) 
                                 (rest lor) 
                                 visisted
                                 cnt)]))]
    (fn-for-room r0 empty empty 0)))
