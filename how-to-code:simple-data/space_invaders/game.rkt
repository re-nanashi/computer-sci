;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; ===========
;; Constants:

(define WIDTH 325)
(define CENTER_X (/ WIDTH 2))
(define HEIGHT 700)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define SPACESHIP (bitmap/file "spaceship.png"))
(define SPACESHIP_POS_Y (- HEIGHT (/ (image-height SPACESHIP) 2)))
(define SPACESHIP_SPEED_X 6)

(define BULLET (bitmap/file "bullet-fire.png"))
(define BULLET_FIRE_POS_Y (- HEIGHT (image-height SPACESHIP)))
(define BULLET_SPEED 20)
(define BULLET_END (- 0 (image-height BULLET)))

(define ENEMY (bitmap/file "enemy.png"))
(define ENEMY_Y_SPEED 5)
(define ENEMY_X_SPEED ENEMY_Y_SPEED)
(define ENEMY_END HEIGHT)
(define ENEMY_SPAWN_X (/ WIDTH 2))
(define ENEMY_SPAWN_Y (- 0 (image-height ENEMY)))
;; 1 = right, -1 left
(define ENEMY_SPAWN_DIR 1)      
(define ENEMY_SPAWN_INTERVAL 1)
(define ENEMY_SPAWN_SPEED 1/28)

;; ==================
;; Data definitions:

(define-struct bullet (x y))
;; Bullet is (make-bullet Natural Natural)
;; interp. a bullet at screen coordinate x, y
(define B0 (make-bullet 100 BULLET_FIRE_POS_Y))
(define B1 (make-bullet 20 200))

#;
(define (fn-for-bullet b)
  (... (bullet-x b)                 ; Natural
       (bullet-y b)))               ; Natural

;; Template rules used:
;; - compound: 2 fields

;; ListOfBullet is one of:
;; - empty
;; - (cons Bullet ListOfBullet)
(define LOB0 empty)
(define LOB1 (cons (make-bullet 20 100) empty))
(define LOB2 (cons (make-bullet 20 100) (cons (make-bullet CENTER_X 100) empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-bullet (first lob))
              (fn-for-lob (rest lob))) ]))

;; Template rules used: 
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Bullet ListOfBullet)
;; - reference: (first lob) is Bullet
;; - self-reference: (rest lob) is ListOfBullet

(define-struct spaceship (x dir bullets))
;; Spaceship is (make-spaceship Natural ListOfBullet)
;; interp. a spaceship at screen coordinate x
;;          dir is the direction of spaceship
;;          bullets is ListOfBullet that is fired by the spaceship
(define SS0 (make-spaceship CENTER_X 1 empty))
(define SS1 (make-spaceship CENTER_X -1 (cons B0 empty)))

#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)                 ; Natural
       (spaceship-dir ss)               ; Integer
       (spaceship-bullets ss)))         ; ListOfBullet

;; Template rules used: 
;; - compound: 3 fields

(define-struct enemy (x y dir))
;; Enemy is (make-enemy Natural Natural Integer)
;; interp. an enemy at x, y screen-coordinate
;;          dir is direction, -1 for left and +1 for right
(define E0 (make-enemy 100 ENEMY_SPAWN_Y ENEMY_SPAWN_DIR))
(define E1 (make-enemy 150 ENEMY_SPAWN_Y ENEMY_SPAWN_DIR))

#;
(define (fn-for-enemy e)
  (... (enemy-x e)                 ; Natural
       (enemy-y e)                 ; Natural
       (enemy-dir e)))             ; Integer

;; Template rules used: 
;; - compound: 4 fields

;; ListofEnemy is one of:
;; - empty
;; - (cons Enemy ListOfEnemy)
(define LOE0 empty)
(define LOE1 (cons E0 empty))
(define LOE2 (cons E0 (cons E1 empty)))

#;
(define (fn-for-loe loe)
  (cond [(empty? loe) (...)]
        [else
         (... (fn-for-enemy (first loe))
              (fn-for-loe (rest loe))) ]))

;; Template rules used: 
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Enemy ListOfEnemy)
;; - reference: (first loe) is Enemy
;; - self-reference: (rest loe) is ListOfEnemy

(define-struct game (spaceship enemies si))
;; SpaceInvaders is (make-game Spaceship ListOfBullet ListofEnemy Integer)
;; interp. a SpaceInvaders game has
;;      spaceship - a Spaceship compound data that is controlled by user
;;      enemies - is ListofEnemy, a compound data type that spawns over time
;;      si - is Spawn interval of enemies
(define G0 (make-game SS0 empty ENEMY_SPAWN_INTERVAL))
(define G1 (make-game SS1 empty ENEMY_SPAWN_INTERVAL))
(define G2 (make-game SS0 LOE1 ENEMY_SPAWN_INTERVAL))

#;
(define (fn-for-game g)
  (... (game-spaceship g)               ; Spaceship
       (game-enemies g)                 ; ListofEnemy
       (game-si g)))                    ; Natural

;; Template rules used: 
;; - compound: 2 fields

(define-struct collided (e b))
;; Collided pari is (make-collided Enemy Bullet)
;; interp. a data type that consists of collided Enemy-Bullet pair
(define C0 (make-collided (make-enemy 100 100 1) (make-bullet 100 100)))
(define C1 (make-collided (make-enemy 200 100 1) (make-bullet 200 100)))

#;
(define (fn-for-c c)
  (... (collided-e c)                   ; Enemy
       (collided-b c)))                 ; Bullet

;; Template rules used: 
;; - compound: 2 fields

;; ListofCollided is one of:
;; - empty
;; (cons Collided ListofCollided)
(define LOC0 empty)
(define LOC1 (cons C0 empty))

#;
(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
          (... (first loc)
               (fn-for-loc (rest loc)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - reference: (first loc) is Collided
;; - self-reference: (rest loc) is ListofCollided

;; ===========
;; Functions:

;; SpaceInvaders -> SpaceInvaders
;; start game with (main G0)
(define (main g)
  (big-bang g                                   ; SpaceInvaders
            (state true)
            (on-tick    advance_handler)        ; SpaceInvaders -> SpaceInvaders
            (to-draw    render)                 ; SpaceInvaders -> Image
            (stop-when  game_end?)              ; SpaceInvaders -> Boolean
            (on-key     handle_key)))           ; SpaceInvaders KeyEvent -> SpaceInvaders

;; SpaceInvaders -> SpaceInvaders
;; advance bullets of the list from spawn to BULLET_END by BULLET_SPEED
(check-random (advance_handler G0) (make-game (make-spaceship (+ CENTER_X SPACESHIP_SPEED_X) 1 empty) 
                                              (cons (make-enemy ENEMY_SPAWN_X ENEMY_SPAWN_Y ENEMY_SPAWN_DIR) empty)
                                              (+ ENEMY_SPAWN_INTERVAL ENEMY_SPAWN_SPEED)))
(check-random (advance_handler G1) (make-game 
                                     (make-spaceship 
                                       (- CENTER_X SPACESHIP_SPEED_X) 
                                       -1 
                                       (cons (make-bullet 100 (- BULLET_FIRE_POS_Y BULLET_SPEED)) empty)) 
                                     (cons (make-enemy ENEMY_SPAWN_X ENEMY_SPAWN_Y ENEMY_SPAWN_DIR) empty)
                                     (+ ENEMY_SPAWN_INTERVAL ENEMY_SPAWN_SPEED)))

(check-random (advance_handler G2) (make-game 
                                     (make-spaceship 
                                       (+ CENTER_X SPACESHIP_SPEED_X) 
                                       1 
                                       empty) 
                                     (cons (make-enemy ENEMY_SPAWN_X ENEMY_SPAWN_Y ENEMY_SPAWN_DIR) 
                                           (cons (make-enemy 100 ENEMY_SPAWN_Y 1) empty))
                                     (+ ENEMY_SPAWN_INTERVAL ENEMY_SPAWN_SPEED)))

;(define (advance_handler g) g)       ; stub
; <template from SpaceInvaders>

;; !!! Create a condition where it checks whether a a bullet and an enemy collides
;; create a function that checks
;; delete both the enemy and the bullet
;;      first create a list of the coordinates to delete
;;      delete on enemies -> new enemy list
;;      delete on bullets -> new bullets list
;;      create a new state according to new list

;(define (advance_handler g)
;  (cond [(check_if_a_bullet_collided 
;           (game-enemies g) 
;           (spaceship-bullets (game-spaceship g)))
;         (create_new_state 
;           (game-spaceship g)
;           (remove_from_game 
;             (create_delete_list ())
;             (game-enemies g) 
;             (spaceship-bullets (game-spaceship g)))
;           (game-si g))]
;        [else
;          (make-game (advance_spaceship (game-spaceship g)) 
;                     (advance_enemies (game-enemies g) (game-si g)) 
;                     (spawn_interval (game-si g)))]))               

(define (advance_handler g)
  (make-game (advance_spaceship (game-spaceship g)) 
             (advance_enemies (game-enemies g) (game-si g)) 
             (spawn_interval (game-si g))))               

;; check: first check if a bullet and an enemy collided
;; bullets enemies -> get the coordinates of the collided and create alist using remove_from_game function
;; list -> using the list as (make-toberemoved enemy bullet)
;; create new state local functions that creates a new list by removing the current

;; ListOfEnemy ListOfBullet -> ListofCollided
;; produce a list of Enemy-Bullet pairs to be deleted
(check-expect (remove_from_game (cons (make-enemy 100 100 1) empty) (cons (make-bullet 100 100) empty)) 
              (cons (make-collided (make-enemy 100 100 1) (make-bullet 100 100)) empty))

(define (remove_from_game loe lob)
  (for ([i loe]) 
    (for ([j loe]) 
      (cond [(compare i j)
             (make-collided i j)]))))

;; Spaceship NewList Interval -> SpaceInvaders
;; remove collided bullets and enemies from respective lists and new game state
(define (create_new_state ss nl si)
  (make-game 
             (make-spaceship 
               (spaceship-x ss) 
               (spaceship-dir ss)
               (nlist-lob nl))
             (nlist-loe nl)
             si))

;; ListOfEnemy ListOfBullet -> Boolean
;; produce true if the x, y coordinates of a bullet on the list hits an enemy
(check-expect (check_if_a_bullet_collided 
                empty empty) false)
(check-expect (check_if_a_bullet_collided 
                empty (cons (make-bullet 205 200) empty)) false)
(check-expect (check_if_a_bullet_collided 
                (cons (make-enemy 200 200 1) empty) empty) false)
(check-expect (check_if_a_bullet_collided 
                (cons (make-enemy 200 200 1) empty) (cons (make-bullet 0 200) empty)) false)
(check-expect (check_if_a_bullet_collided 
                (cons (make-enemy 200 200 1) empty) (cons (make-bullet 205 200) empty)) true)
(check-expect (check_if_a_bullet_collided 
                (cons (make-enemy 200 200 1) empty) (cons (make-bullet 0 200) (cons (make-bullet 205 200) empty))) true)


;(define (check_if_a_bullet_collided loe lob) false)        ;stub

(define (check_if_a_bullet_collided loe lob)
  (cond [(empty? loe) false]
        [else
          (or  (check_if_x_is_near (first loe) lob) 
               (check_if_a_bullet_collided (rest loe) lob))]))

;; Enemy ListOfBullet -> Boolean
;; produce true if (enemy-y) == (bullet-y) && (enemy-x) +/- (image-width ENEMY) is 
;; near a bullet's x coordinate inside a list 
(check-expect (check_if_x_is_near (make-enemy 200 200 1) empty) false)
(check-expect (check_if_x_is_near 
                (make-enemy 200 200 1) 
                (cons (make-bullet 0 200) (cons (make-bullet 205 200) empty)))true)

;(define (check_if_x_is_near e lob) false)       ; stub

(define (check_if_x_is_near e lob) 
  (cond [(empty? lob) false]
        [else
          (or  (compare (first lob) e) 
               (check_if_x_is_near e (rest lob)))]))

;; Bullet Enemy -> Boolean
;; produce true when bullet's x,y and enemy's x,y is near
(check-expect (compare (make-bullet 0 200) (make-enemy 200 200 1)) false)
(check-expect (compare (make-bullet 200 200) (make-enemy 200 200 1)) true)
(check-expect (compare (make-bullet 195 200) (make-enemy 200 200 1)) true)
(check-expect (compare (make-bullet 205 200) (make-enemy 200 200 1)) true)

(define (compare b e)
  (and (equal? (bullet-y b) (enemy-y e)) 
       (<= (- (enemy-x e) (image-width ENEMY))
           (bullet-x b)
           (+ (enemy-x e) (image-width ENEMY)))))

;; Spaceship -> Spaceship
;; move spaceship to its direction by by SPACESHIP_SPEED_X
;; and advance the bullets
(check-expect (advance_spaceship SS0) (make-spaceship (+ CENTER_X (* (spaceship-dir SS0) SPACESHIP_SPEED_X)) 1 empty))
(check-expect (advance_spaceship SS1) (make-spaceship 
                                        (+ CENTER_X (* (spaceship-dir SS1) SPACESHIP_SPEED_X)) 
                                        -1 
                                        (cons (make-bullet 100 (- BULLET_FIRE_POS_Y BULLET_SPEED)) empty)))

;(define (advance_spaceship ss) ss)      ; stub
; <template from SPACESHIP>

(define (advance_spaceship ss)
  (make-spaceship (advance_spaceship_position (spaceship-x ss) (spaceship-dir ss))                 
                  (update_spaceship_dir (spaceship-x ss) (spaceship-dir ss))              
                  (advance_bullets (spaceship-bullets ss))))       

;; (spaceship-x)::Natural (spaceship-dir)::Integer -> Natural
;; produce a new x coordinate according to its current position and direction
(check-expect (advance_spaceship_position (spaceship-x SS0) (spaceship-dir SS0)) 
              (+ (spaceship-x SS0) (* (spaceship-dir SS0) SPACESHIP_SPEED_X)))
(check-expect (advance_spaceship_position (spaceship-x SS1) (spaceship-dir SS1)) 
              (+ (spaceship-x SS1) (* (spaceship-dir SS1) SPACESHIP_SPEED_X)))

;(define (advance_spaceship_position x d) 0)     ; stub

(define (advance_spaceship_position x d)
  (+ x (* d SPACESHIP_SPEED_X)))

;; (spaceship-x)::Natural (spaceship-dir)::Integer -> Integer
;; produce the new direction of the spaceship if it bumps the edge, else do nothing
(check-expect (update_spaceship_dir 100 1) 1)
(check-expect (update_spaceship_dir 100 -1) -1)
(check-expect (update_spaceship_dir 0 -1) 1)
(check-expect (update_spaceship_dir WIDTH 1) -1)

;(define (update_spaceship_dir x d) -1)      ; stub

(define (update_spaceship_dir x d)
  (cond [(>= (+ x SPACESHIP_SPEED_X) (- WIDTH (/ (image-width SPACESHIP) 2))) -1]
        [(<= (- x SPACESHIP_SPEED_X) (/ (image-width SPACESHIP) 2)) 1]
        [else d]))

;; ListOfBullet -> ListOfBullet
;; advance bullets of the list from spawn to BULLET_END by BULLET_SPEED
(check-expect (advance_bullets (spaceship-bullets SS0)) empty)
(check-expect (advance_bullets LOB2) 
              (cons (make-bullet 20 (- 100 BULLET_SPEED)) 
                    (cons (make-bullet CENTER_X (- 100 BULLET_SPEED)) empty)))
(check-expect (advance_bullets (spaceship-bullets SS1)) 
              (cons (make-bullet 100 (- BULLET_FIRE_POS_Y BULLET_SPEED)) empty))

;(define (advance_bullets lob) lob)      ; stub
; <template from ListOfBullet>

(define (advance_bullets lob)
  (cond [(empty? lob) empty]
        [(has_a_bullet_to_be_deleted? lob)
         (delete_bullet lob)]
        [else
          (cons (advance_bullet (first lob))
                (advance_bullets (rest lob))) ]))

;; ListOfBullet -> Boolean
;; produce true if a drop on the list is subject to deletion
;; by checking if it's y-coordinate is greater than BULLET_END
(check-expect (has_a_bullet_to_be_deleted? empty) false)        ; base
(check-expect (has_a_bullet_to_be_deleted? (cons (make-bullet 10 100) empty)) false)
(check-expect (has_a_bullet_to_be_deleted? (cons (make-bullet 10 BULLET_END) empty)) false)
(check-expect (has_a_bullet_to_be_deleted? (cons (make-bullet 10 20) (cons (make-bullet 10 30) empty))) false)

;(define (has_a_bullet_to_be_deleted? lod) empty)    ; stub
; <template from ListOfBullet>

(define (has_a_bullet_to_be_deleted? lob)
  (cond [(empty? lob) false]
        [else
         (or  (delete? (first lob))
              (has_a_bullet_to_be_deleted? (rest lob))) ]))

;; Bullet -> Boolean
;; produce true if the bullet's y-coordinate is less BULLET_END
(check-expect (delete? (make-bullet 100 (- BULLET_END 1))) true)
(check-expect (delete? (make-bullet 100 200)) false)
(check-expect (delete? (make-bullet 100 BULLET_END)) false)

(define (delete? b)
  (< (bullet-y b) BULLET_END))

;; ListOfBullet -> ListOfBullet
;; delete a bullet from list if a bullet is subject to deletion
(check-expect (delete_bullet empty) empty)        ; base
(check-expect (delete_bullet (cons (make-bullet 10 100) empty)) (cons (make-bullet 10 100) empty))
(check-expect (delete_bullet (cons (make-bullet 10 (- BULLET_END 1)) empty)) empty)
(check-expect (delete_bullet (cons (make-bullet 10 (- BULLET_END 1)) (cons (make-bullet 10 30) empty))) (cons (make-bullet 10 30) empty))

;(define (delete_bullet lob) lob)    ;stub
; <template from ListOfBullet>

(define (delete_bullet lob)
  (cond [(empty? lob) empty]
        [else
         (cond [(delete? (first lob)) 
                (delete_bullet (rest lob))]
               [else
                 (cons (first lob) 
                       (delete_bullet (rest lob)))])]))

;; Bullet -> Bullet
;; advance bullet by BULLET_SPEED
(check-expect (advance_bullet B0) (make-bullet 100 (- BULLET_FIRE_POS_Y BULLET_SPEED)))
(check-expect (advance_bullet B1) (make-bullet 20 (- 200 BULLET_SPEED)))

;(define (advance_bullet b) b)   ; stub
; <template from Bullet>

(define (advance_bullet b)
  (make-bullet (bullet-x b) (- (bullet-y b) BULLET_SPEED)))

;; ListofEnemy Integer -> ListofEnemy
;; produce enemies every second from the ENEMY_SPAWN_Y and at random X 
;; advance every enemy on the list
(check-random (advance_enemies LOE0 1) (cons (make-enemy ENEMY_SPAWN_X ENEMY_SPAWN_Y ENEMY_SPAWN_DIR) empty))
(check-expect (advance_enemies LOE0 1/28) empty)
(check-random (advance_enemies LOE1 1) 
              (cons (make-enemy ENEMY_SPAWN_X ENEMY_SPAWN_Y ENEMY_SPAWN_DIR) 
                    (cons (make-enemy 100 ENEMY_SPAWN_Y ENEMY_SPAWN_DIR) empty)))
(check-expect (advance_enemies LOE1 1/28) 
              (cons (make-enemy (+ 100 ENEMY_X_SPEED) (+ ENEMY_SPAWN_Y ENEMY_Y_SPEED) ENEMY_SPAWN_DIR) empty))

;(define (advance_enemies loe) loe)  ; stub
; <template from ListofEnemy>

(define (advance_enemies loe i)
  (cond [(integer? i) 
         (cons (make-enemy (spawn_enemy_x i) ENEMY_SPAWN_Y ENEMY_SPAWN_DIR) loe)]
        [else
          (cond [(empty? loe) empty]
                [else
                  (cons (advance_enemy (first loe)) 
                        (advance_enemies (rest loe) i))])]))


;; Enemy -> Enemy
;; advance enemy position by adding ENEMY_Y_SPEED and ENEMY_X_SPEED to x and y
(check-expect (advance_enemy E0) (make-enemy (+ 100 ENEMY_X_SPEED) (+ ENEMY_SPAWN_Y ENEMY_Y_SPEED) ENEMY_SPAWN_DIR))

;(define (advance_enemy e) e)    ; stub

(define (advance_enemy e)
  (make-enemy 
    (+ (enemy-x e) (* (enemy-dir e) ENEMY_X_SPEED))
    (+ (enemy-y e) ENEMY_Y_SPEED)
    (update_enemy_dir (enemy-x e) (enemy-dir e))))

;; Integer -> Integer
;; update (enemy-dir) when enemy bumps the side of the screen
(check-expect (update_enemy_dir 100 1) 1)
(check-expect (update_enemy_dir 100 -1) -1)
(check-expect (update_enemy_dir 0 -1) 1)
(check-expect (update_enemy_dir WIDTH 1) -1)

;(define (update_enemy_dir x dir) dir)     ;stub

(define (update_enemy_dir x d) 
  (cond [(>= (+ x ENEMY_X_SPEED) (- WIDTH (/ (image-width ENEMY) 2))) -1]
        [(<= (- x SPACESHIP_SPEED_X) (/ (image-width ENEMY) 2)) 1]
        [else d]))

;; Integer -> Integer
;; add ENEMY_SPAWN_SPEED to interval
(check-expect (spawn_interval ENEMY_SPAWN_INTERVAL) (+ ENEMY_SPAWN_INTERVAL ENEMY_SPAWN_SPEED))
(check-expect (spawn_interval 0.5) (+ 0.5 ENEMY_SPAWN_SPEED))

;(define (spawn_interval i) 0)   ; stub

(define (spawn_interval i) 
  (+ i ENEMY_SPAWN_SPEED))

;; Integer -> Integer[0, WIDTH]
;; produce a new enemy spawn x coordinate
(check-expect (spawn_enemy_x 1) ENEMY_SPAWN_X)

;(define (spawn_enemy_x i) 0)   ;stub

(define (spawn_enemy_x i) 
  (cond [(= 1 i) ENEMY_SPAWN_X]
        [else
          (random WIDTH)]))

;; SpaceInvaders -> Image
;; render an image according to (spaceship-x (game-spaceship g))
(check-expect (render G0) (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS)) 

;(define (render g) empty-scene)     ; stub
; <template from SpaceInvaders>

(define (render g)
  (render_game_objects 
    (game-spaceship g)
    (game-enemies g)))               

;; Spaceship ListOfEnemy -> Image
;; render ListOfEnemy to spaceship image 
(define (render_game_objects ss loe)
  (cond [(empty? loe) (render_spaceship ss)]
        [else
          (render_enemies (first loe) 
                          (render_game_objects ss (rest loe)))])) 
;; Enemy Image -> Image
;; render Enemy into an Image
(check-expect (render_enemies E0 MTS) (place-image ENEMY 100 ENEMY_SPAWN_Y MTS))
(check-expect (render_enemies E0 MTS) (place-image ENEMY 150 ENEMY_SPAWN_Y MTS))

;(define (render_enemies e img) (empty-scene))      ; stub

(define (render_enemies e img)
  (place-image ENEMY (enemy-x e) (enemy-y e) img))

;; Spaceship -> Image
;; render spaceship into an Image and Place it into MTS
(check-expect (render_spaceship (make-spaceship CENTER_X 1 empty)) 
              (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS))
(check-expect (render_spaceship (make-spaceship CENTER_X 1 (cons (make-bullet CENTER_X BULLET_FIRE_POS_Y) empty)))
              (place-image BULLET CENTER_X BULLET_FIRE_POS_Y
                           (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS)))

;(define (render_spaceship s) MTS)        ; stub
; <template from Spaceship>

(define (render_spaceship s)
  (render_bullets (spaceship-bullets s) 
                  (spaceship-x s)))

;; ListOfBullet Natural -> Image
;; render bullets from list to the spaceship image at x
(check-expect (render_bullets empty CENTER_X) (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS))
(check-expect (render_bullets (cons (make-bullet 20 100) empty) CENTER_X) 
              (place-image BULLET 20 100 (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS)))

;(define (render_bullets lob x) (empty-scene))    ; stub
; <template from ListOfBullet>

(define (render_bullets lob x)
  (cond [(empty? lob) (place-image SPACESHIP x SPACESHIP_POS_Y MTS)]
        [else
          (render_bullet (first lob) 
                         (render_bullets (rest lob) x))]))

;; Bullet Image -> Image
;; render bullet to an Image
(check-expect (render_bullet B0 MTS) (place-image BULLET 100 BULLET_FIRE_POS_Y MTS))
(check-expect (render_bullet B1 MTS) (place-image BULLET 20 200 MTS))

;(define (render_bullet b) MTS)   ;stub
; <template from Bullet>

(define (render_bullet b img)
  (place-image BULLET (bullet-x b) (bullet-y b) img))

;; SpaceInvaders KeyEvent -> SpaceInvaders
;; change the Spaceship's direction with the left and right arrow keys
(check-expect (handle_key G0 "right") 
              (make-game (make-spaceship CENTER_X 1 empty) (game-enemies G0) (game-si G0)))

(check-expect (handle_key G0 "left") 
              (make-game (make-spaceship CENTER_X -1 empty) (game-enemies G0) (game-si G0)))

(check-expect (handle_key G0 " ") 
              (make-game (make-spaceship 
                           CENTER_X 
                           1
                           (cons (make-bullet (spaceship-x (game-spaceship G0)) BULLET_FIRE_POS_Y) empty)) 
                         (game-enemies G0) 
                         (game-si G0)))

;(define (handle_key g key) g)       ; stub
; <template from KeyEvent>

(define (handle_key g key)
  (cond [(key=? key "right") 
         (make-game 
           (change_spaceship_dir (game-spaceship g) 1)
           (game-enemies g)
           (game-si g))]
        [(key=? key "left") 
         (make-game 
           (change_spaceship_dir (game-spaceship g) -1)
           (game-enemies g)
           (game-si g))]
        [(key=? key " ") 
         (make-game 
           (fire_bullet (game-spaceship g))
           (game-enemies g)
           (game-si g))]
        [else 
          g]))

;; Spaceship Integer -> Spaceship
;; change spaceship's direction by multiplying (spaceship-dir) by given integer::direction
(check-expect (change_spaceship_dir SS0 -1) (make-spaceship CENTER_X -1 empty))
(check-expect (change_spaceship_dir SS1 1) (make-spaceship CENTER_X 1 (cons B0 empty)))

;(define (change_spaceship_dir ss) ss)   ; stub
; <template from Spaceship>

(define (change_spaceship_dir ss d)
  (make-spaceship (spaceship-x ss) 
                  d
                  (spaceship-bullets ss)))

;; Spaceship -> Spaceship
;; fire bullets by adding a Bullet to the ListOfBullet
(check-expect (fire_bullet SS0) 
              (make-spaceship (spaceship-x SS0)
                              (spaceship-dir SS0)
                              (cons 
                                (make-bullet 
                                  (spaceship-x SS0)
                                  BULLET_FIRE_POS_Y)
                                empty)))

(check-expect (fire_bullet SS1) 
              (make-spaceship (spaceship-x SS1)
                              (spaceship-dir SS1)
                              (cons (make-bullet (spaceship-x SS1) BULLET_FIRE_POS_Y) 
                                    (cons B0 empty))))

;(define (fire_bullet ss) ss)            ; stub
; <template from Spaceship>

(define (fire_bullet ss)
  (make-spaceship (spaceship-x ss)                 
                  (spaceship-dir ss)              
                  (add_bullet ss)))    

;; Spaceship -> ListOfBullet
;; add bullets to the (spaceship-bullets) at the current SPACESHIP_POS_X 
(check-expect (add_bullet SS0) 
              (cons (make-bullet (spaceship-x SS0) BULLET_FIRE_POS_Y) empty))
(check-expect (add_bullet SS1) 
              (cons (make-bullet (spaceship-x SS1) BULLET_FIRE_POS_Y) (cons B0 empty)))

;(define (add_bullet ss) lob)    ; stub
; <template from Spaceship>

(define (add_bullet ss)
  (cons (make-bullet (spaceship-x ss) BULLET_FIRE_POS_Y) 
       (spaceship-bullets ss)))

;; SpaceInvaders -> Boolean
;; stop the game when an enemy crosses the end 
(define (game_end? g)
  (check_if_game_ended (game-enemies g)))

;; ListofEnemy -> Boolean
;; produce true if an enemy crossed_the_line
(define (check_if_game_ended loe)
  (cond [(empty? loe) false]
        [else
          (or (crossed_the_line? (first loe))
              (check_if_game_ended (rest loe)))]))

;; Enemy -> Boolean
;; produce true if enemy-y is greater or equal to default enemy end
(define (crossed_the_line? e)
  (>= (enemy-y e) ENEMY_END))


