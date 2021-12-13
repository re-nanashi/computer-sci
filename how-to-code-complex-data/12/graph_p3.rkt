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

;; Rooms String -> Room or false
;; produce the room with the given name, false otherwise
(check-expect (lookup-room H1 "A") H1)
(check-expect (lookup-room H3 "C") (shared ((-C- (make-room "C" 
                                                            (list (make-room "A" 
                                                                             (list (make-room "B" 
                                                                                              (list -C-))))))))
                                     -C-))
(check-expect (lookup-room H4 "F") (make-room "F" empty)) 
(check-expect (lookup-room H4 "X") false)


(define (lookup-room r0 rn)
  (local [(define (fn-for-room r todo visisted)
            (cond [(string=? (room-name r) rn) r]
                  [(member (room-name r) visisted)
                   (fn-for-lor todo visisted)]
                  [else 
                    (fn-for-lor (append (room-exits r) todo) 
                                (cons (room-name r) visisted))]))

          (define (fn-for-lor lor visisted)
            (cond [(empty? lor) false]
                  [else 
                    (fn-for-room (first lor) 
                                 (rest lor) 
                                 visisted) ]))]
    (fn-for-room r0 empty empty)))
