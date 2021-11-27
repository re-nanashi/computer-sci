;; Data Definitions:

(define-struct chirper (name verified following))
;; Chirper is (make-chirper Name Boolean (listof Chirper))
;; interp. the chirper's name, his/her verification, and list of users he/she follows.

(define C1 (make-chirper "Juan" false (list (make-chirper "Bon" false empty))))
(define C2 
  (shared ((-0- (make-chirper "Juan" false (list (make-chirper "Bon" false (list -0-))))))
          -0-))
(define C3
  (shared ((-1- (make-chirper "Juan" false (list -2-)))
           (-2- (make-chirper "Bon" false (list -3-)))
           (-3- (make-chirper "Yu" true (list -1- -2-))))
          -1-))
(define C4
  (shared ((-1- (make-chirper "Juan" false (list -2- -4-)))
           (-2- (make-chirper "Bon" false (list -3- -5-)))
           (-3- (make-chirper "Yu" true (list -1- -2-)))
           (-4- (make-chirper "Mina" true (list -1- -5-)))
           (-5- (make-chirper "Momo" true (list -6- -1-)))
           (-6- (make-chirper "Yama" false (list))))
          -1-))

;; template; structural recursion, encapsulate w/ local, tail-recursive 
;;           context-preserving accumulator on who follows who

#;
(define (fn-for-chirper c0)
  (local [(define (fn-for-chirper c todo visited)
            (if (member (chirper-name c) visited)
              (fn-for-loc todo visited)
              (fn-for-loc (append (chirper-following c) todo)
                          (cons (chirper-name c) visited))))

          (define (fn-for-loc todo visited)
            (cond [(empty? todo) (...)]
                  [else
                    (fn-for-chirper (first todo) 
                                    (rest todo)
                                    visited)]))]
    (fn-for-chirper c0 empty empty)))

;; Chirper -> Chirper
;; produce the chirper that has the most followers.
;; In case of a tie, produce either one.
(check-expect (most_followers C1) (first (chirper-following C1)))
(check-expect (most_followers C2) C2)
(check-expect (most_followers (shared ((-1- (make-chirper "Juan" false (list -2-)))
                                       (-2- (make-chirper "Bon" false (list -3-)))
                                       (-3- (make-chirper "Yu" true (list -1- -2-))))
                                      -1-)) 
              (shared ((-1- (make-chirper "Juan" false (list -2-)))
                                       (-2- (make-chirper "Bon" false (list -3-)))
                                       (-3- (make-chirper "Yu" true (list -1- -2-))))
                                      -2-))
(check-expect (most_followers C4) C4)

(define (most_followers c0)
  ;; !!! not name but entry itself
  (local [(define-struct entry (chirper no_of_followers))
          ;; FollowersEntry is (make-entry Chirper Natural)

          ;; (listof Chirper) (listof FollowersEntry) -> (listof FollowersEntry)
          ;; given a list of chirper, update the (listof FollowersEntry) accumulator.
          ;; accumulator: follower_count
          (define (create_follower_entry follow_list0 follower_count0) 
            (local [(define (create_follower_entry follow_list follower_count)
                     (cond [(empty? follow_list) follower_count] 
                           [else 
                             (create_follower_entry (rest follow_list) 
                                                    (update_list (first follow_list) 
                                                                 follower_count))]))]
              (create_follower_entry follow_list0 follower_count0)))

          ;; Chirper (listof FollowersEntry) -> (listof FollowersEntry)
          ;; update the list by adding an entry or incrementing an entry's value.
          (define (update_list entry0 lst0)
            (local [(define entry_already_exists 
                      (ormap (lambda (x) 
                               (string=? (chirper-name entry0) 
                                         (chirper-name (entry-chirper x)))) 
                             lst0))

                    (define (increment lst1)
                      (local [(define (increment lst updated_list)
                                (cond [(empty? lst) updated_list]
                                      [else 
                                        (increment 
                                          (rest lst) 
                                          (if (string=? (chirper-name entry0) 
                                                        (chirper-name (entry-chirper (first lst)))) 
                                            (cons (make-entry (entry-chirper (first lst)) 
                                                              (add1 (entry-no_of_followers (first lst)))) 
                                                  updated_list) 
                                            (cons (first lst) updated_list)))]))]
                        (increment lst1 empty)))]
              (if entry_already_exists
                (increment lst0)
                (cons (make-entry entry0 1) lst0))))
          
          (define (find_most follower_count0)
            (local [(define (find_most follower_count most)
                      (cond [(empty? follower_count) most] 
                            [else
                              (find_most (rest follower_count)
                                         (if (> (entry-no_of_followers (first follower_count)) 
                                                (entry-no_of_followers most))
                                           (first follower_count)
                                           most))]))]
              (find_most follower_count0 (make-entry (make-chirper "" false empty) 0))))

          (define (fn-for-chirper c todo visited follower_count)
            (if (member (chirper-name c) visited)
              (fn-for-loc todo visited follower_count)
              (fn-for-loc (append (chirper-following c) todo)
                          (cons (chirper-name c) visited)
                          (create_follower_entry (chirper-following c)
                                                 follower_count))))

          (define (fn-for-loc todo visited follower_count)
            (cond [(empty? todo) (entry-chirper (find_most follower_count))]
                  [else
                    (fn-for-chirper (first todo) 
                                    (rest todo)
                                    visited
                                    follower_count)]))]
    (fn-for-chirper c0 empty empty empty)))

;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2) (list 1 3))
(define UDON (make-ta "Udon" 1) (list 3 4))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE_TAs (list SOBA UDON RAMEN))

(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)

;; Functions
;; (listof TA) (listof Slot) -> Schedule or false
;; produce valide schedule given TAs and Slots; false if impossible
(check-expect (schedule_tas empty empty) empty)
(check-expect (schedule_tas empty (list 1 2)) false)
(check-expect (schedule_tas (list SOBA) empty) empty)
(check-expect (schedule_tas (list SOBA UDON) (list 1 3 4)) 
              (list (make-assignment UDON 4) 
                    (make-assignment SOBA 3) 
                    (make-assignment SOBA 1)))

(check-expect (schedule_tas (list SOBA) (list 1))
              (list (make-assignment SOBA 1)))
(check-expect (schedule_tas (list SOBA) (list 2)) false)
(check-expect (schedule_tas (list SOBA) (list 1 3)) 
              (list (make-assignment SOBA 3)
                    (make-assignment SOBA 1)))
(check-expect (schedule_tas NOODLE_TAs (list 1 2 3 4)) 
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule_tas NOODLE_TAs (list 1 2 3 4 5)) false)

;(define (schedule_tas tas slots) empty) ;stub
;; !!! next_los
;; next_los should create a list of assignemnts using tail recursion and accumulator 
;; next_los should be able to generate valid list of assignments from ( (list 1 3 4)) empty 
;; (list 3 4) (list of assignments with all 1 )

;; !!! solved? 


;<template: recursion, tail recursion, encapsulate w/ local 

(define (schedule_tas tas slots)
  (local [
          ;; (slot=empty)
          ;; find until length of slot is equal to length of the given slots (list) << this
          ;; find the the next step or the next move  << the problem
          ;; fill it with available ta slot
          ;; keep only valid from list
          (define (next_los slot0)
            (local [
                    (define (next_los slot))]

              (next_los slot0 )))

          (define (fn-for-slots slot)
            (cond [(solved? slot) slot]
                  [else
                    (fn-for-los (next_los slot))]))

          (define (fn-for-los los)
            (cond [(empty? los) false]
                  [else
                    (local [(define try (fn-for-slots (first los)))]
                      (if (not (false? try))
                        try
                        (fn-for-los (rest los))))]))]
    (fn-for-slots empty)))
