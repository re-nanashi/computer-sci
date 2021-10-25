(require 2htdp/image)
(require 2htdp/universe)
;; Add spaceship X position tick
;; Add enemies

;; ===========
;; Constants:
;; !!! 

(define WIDTH 325)
(define CENTER_X (/ WIDTH 2))
(define HEIGHT 700)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define SPACESHIP (bitmap/file "spaceship.png"))
(define SPACESHIP_POS_Y (- HEIGHT (/ (image-height SPACESHIP) 2)))
(define SPACESHIP_SPEED_X 8)

(define BULLET (bitmap/file "bullet-fire.png"))
(define BULLET_FIRE_POS_Y (- HEIGHT (image-height SPACESHIP)))
(define BULLET_SPEED 20)
(define BULLET_END (- 0 (image-height BULLET)))

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

;;(define-struct enemy)
;; !!! create compound enemy and ListOfEnemy

(define-struct game (spaceship enemies))
;; SpaceInvaders is (make-game Spaceship ListOfBullet ListofEnemy)
;; interp. a SpaceInvaders game has
;;      spaceship - a Spaceship compound data that is controlled by user
;;      enemies - is ListofEnemy, a compound data type that spawns over time
(define G0 (make-game SS0 empty))
(define G1 (make-game SS1 empty))

#;
(define (fn-for-game g)
  (... (game-spaceship g)               ; Spaceship
       (game-enemies g)))               ; ListofEnemy

;; Template rules used: 
;; - compound: 2 fields

;; ===========
;; Functions:

;; SpaceInvaders -> SpaceInvaders
;; start game with (main G0)
;; !!! Create handlers
(define (main g)
  (big-bang g                                   ; SpaceInvaders
            (state true)
            (on-tick    advance_handler)        ; SpaceInvaders -> SpaceInvaders
            (to-draw    render)                 ; SpaceInvaders -> Image
            ;(stop-when  ...)                   ; SpaceInvaders -> Boolean
            (on-key     handle_key)))           ; SpaceInvaders KeyEvent -> SpaceInvaders

;; SpaceInvaders -> SpaceInvaders
;; advance bullets of the list from spawn to BULLET_END by BULLET_SPEED
(check-expect (advance_handler G0) (make-game (make-spaceship (+ CENTER_X SPACESHIP_SPEED_X) 1 empty) empty))
(check-expect (advance_handler G1) (make-game 
                                     (make-spaceship 
                                       (- CENTER_X SPACESHIP_SPEED_X) 
                                       -1 
                                       (cons (make-bullet 100 (- BULLET_FIRE_POS_Y BULLET_SPEED)) empty)) 
                                     empty))

;(define (advance_handler g) g)       ; stub
; <template from SpaceInvaders>

;; !!! make advance enemies
;(define (advance_handler g)
;  (make-game (advance_spaceship (game-spaceship g)) 
;             (advance_enemies (game-enemies g))))               

(define (advance_handler g)
  (make-game (advance_spaceship (game-spaceship g)) 
             empty))               


;; Spaceship -> Spaceship
; !!! Spaceship on tick and advance bullets
;; instead of in the keyboard handler that can go to direction change it into advance handler
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
  (cond [(> (+ x SPACESHIP_SPEED_X) (- WIDTH (/ (image-width SPACESHIP) 2))) -1]
        [(< (- x SPACESHIP_SPEED_X) (/ (image-width SPACESHIP) 2)) 1]
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

;; SpaceInvaders -> Image
;; render an image according to (spaceship-x (game-spaceship g))
;; !!! render all images from Spaceship to Enemies
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
;;!!! render_enemies
;; Enemy -> 
(define (render_enemies s d) (empty-scene))

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
;; add argument for enemies !!!
(check-expect (handle_key G0 "right") 
              (make-game (make-spaceship CENTER_X 1 empty) empty))

(check-expect (handle_key G0 "left") 
              (make-game (make-spaceship CENTER_X -1 empty) empty))

(check-expect (handle_key G0 " ") 
              (make-game (make-spaceship 
                           CENTER_X 
                           1
                           (cons (make-bullet (spaceship-x (game-spaceship G0)) BULLET_FIRE_POS_Y) empty)) 
                         empty))

;(define (handle_key g key) g)       ; stub
; <template from KeyEvent>

(define (handle_key g key)
  (cond [(key=? key "right") 
         (make-game 
           (change_spaceship_dir (game-spaceship g) 1)
           empty)]
        [(key=? key "left") 
         (make-game 
           (change_spaceship_dir (game-spaceship g) -1)
           empty)]
        [(key=? key " ") 
         (make-game 
           (fire_bullet (game-spaceship g))
           empty)]
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

;(define G0 (make-game SS0 empty))
;(define G1 (make-game SS1 empty))
;(define B0 (make-bullet 100 BULLET_FIRE_POS_Y))
;(define B1 (make-bullet 20 200))
;(define SS0 (make-spaceship CENTER_X 1 empty))
;(define SS1 (make-spaceship CENTER_X -1 (cons B0 empty)))
;(define LOB0 empty)
;(define LOB1 (cons (make-bullet 20 100) empty))
;(define LOB2 (cons (make-bullet 20 100) (cons (make-bullet CENTER_X 100) empty)))

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
