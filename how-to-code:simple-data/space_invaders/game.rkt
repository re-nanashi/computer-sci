;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

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

(define-struct spaceship (x bullets))
;; Spaceship is (make-spaceship Natural ListOfBullet)
;; interp. a spaceship at screen coordinate x
;;          bullets is ListOfBullet that is fired by the spaceship
(define SS0 (make-spaceship CENTER_X empty))
(define SS1 (make-spaceship CENTER_X (cons B0 empty)))

#;
(define (fn-for-spaceship ss)
  (... (spaceship-x ss)                 ; Natural
       (spaceship-bullets ss)))         ; ListOfBullet

;; Template rules used: 
;; - compound: 2 fields

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
  (big-bang g                           ; SpaceInvaders
            (state true)
            (on-tick    advance_handler)            ; SpaceInvaders -> SpaceInvaders
            (to-draw    render)            ; SpaceInvaders -> Image
            ;(stop-when  ...)              ; SpaceInvaders -> Boolean
            (on-key     handle_key)))          ; SpaceInvaders KeyEvent -> SpaceInvaders

;; SpaceInvaders -> SpaceInvaders
;!!! NO DELETE FIRST, CHANGE NAME LATER PARA DI MALITO
;; advance bullets of the list from spawn to BULLET_END by BULLET_SPEED
(check-expect (advance_handler G0) G0)
(check-expect (advance_handler G1) (make-game 
                                     (make-spaceship 
                                       CENTER_X 
                                       (cons (make-bullet 100 (- BULLET_FIRE_POS_Y BULLET_SPEED)) empty))
                                     empty))

;(define (advance_handler g) g)       ; stub
; <template from SpaceInvaders>

(define (advance_handler g) 
  (make-game 
    (make-spaceship (get_spaceship_pos g) 
                    (advance_bullets (spaceship-bullets (game-spaceship g))))
    empty))

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
        [else
          (cons (advance_bullet (first lob))
                (advance_bullets (rest lob))) ]))

;;(define LOB2 (cons (make-bullet 20 100) (cons (make-bullet CENTER_X 100) empty)))
;;(define G0 (make-game SS0 empty))
;;(define G1 (make-game SS1 empty))
;;(define SS0 (make-spaceship CENTER_X empty))
;;(define SS1 (make-spaceship CENTER_X (cons B0 empty)))
;;(define B0 (make-bullet 100 SPACESHIP_POS_Y))
;;(define B1 (make-bullet 20 200))

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

;(define (render g)
;  (... (game-spaceship g)               
;       (game-enemies g)))              

(define (render g)
  (renderGameObjects 
    (game-spaceship g)
    (game-enemies g)))               

;; ListOfEnemy Spaceship -> Image
;; render ListOfEnemy to spaceship image 
(define (renderGameObjects ss loe)
  (cond [(empty? loe) (renderSpaceship ss)]
        [else
          (renderEnemies (first loe)
                         (renderGameObjects ss (rest loe)))])) 
;;!!! renderEnemies
;; Enemy -> 
(define (renderEnemies s d) (empty-scene))
;; Spaceship -> Image
;; render spaceship into an Image and Place it into MTS
(check-expect (renderSpaceship (make-spaceship CENTER_X empty)) 
              (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS))
(check-expect (renderSpaceship (make-spaceship CENTER_X (cons (make-bullet CENTER_X BULLET_FIRE_POS_Y) empty)))
              (place-image BULLET CENTER_X BULLET_FIRE_POS_Y
                           (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS)))

;(define (renderSpaceship s) MTS)        ; stub
; <template from Spaceship>

(define (renderSpaceship s)
  (renderBullets (spaceship-bullets s)
                 (spaceship-x s)))

;; ListOfBullet Natural -> Image
;; render bullets from list to the spaceship image at x
(check-expect (renderBullets empty CENTER_X) (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS))
(check-expect (renderBullets (cons (make-bullet 20 100) empty) CENTER_X) 
              (place-image BULLET 20 100 (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS)))

;(define (renderBullets lob x) (empty-scene))    ; stub
; <template from ListOfBullet>

(define (renderBullets lob x)
  (cond [(empty? lob) (place-image SPACESHIP x SPACESHIP_POS_Y MTS)]
        [else
          (renderBullet (first lob)
                        (renderBullets (rest lob) x))]))

;; Bullet Image -> Image
;; render bullet to an Image
(check-expect (renderBullet B0 MTS) (place-image BULLET 100 BULLET_FIRE_POS_Y MTS))
(check-expect (renderBullet B1 MTS) (place-image BULLET 20 200 MTS))

;(define (renderBullet b) MTS)   ;stub
; <template from Bullet>

(define (renderBullet b img)
  (place-image BULLET (bullet-x b) (bullet-y b) img))

;; SpaceInvaders KeyEvent -> SpaceInvaders
;; move Spaceship by adding or subtract SPACESHIP_SPEED_X to/from current x
;; !!! add argument for lists and spacebar
(check-expect (handle_key G0 "right") 
              (make-game (make-spaceship (+ CENTER_X SPACESHIP_SPEED_X) empty) empty))

(check-expect (handle_key G0 "left") 
              (make-game (make-spaceship (- CENTER_X SPACESHIP_SPEED_X) empty) empty))

(check-expect (handle_key G0 " ") 
              (make-game (make-spaceship 
                           CENTER_X 
                           (cons (make-bullet (get_spaceship_pos G0) BULLET_FIRE_POS_Y) empty)) 
                         empty))

;(define (handle_key g key) g)       ; stub
; <template from KeyEvent>

(define (handle_key g key)
  (cond [(and (key=? key "right") 
              (can_go_to_dir (get_spaceship_pos g) key)) 
         (make-game (make-spaceship 
                      (+ (get_spaceship_pos g) SPACESHIP_SPEED_X)
                      (spaceship-bullets (game-spaceship g)))         ; ListOfBullet
                    empty)]          ; ListofEnemy
        [(and (key=? key "left") 
              (can_go_to_dir (get_spaceship_pos g) key))
         (make-game (make-spaceship 
                      (- (get_spaceship_pos g) SPACESHIP_SPEED_X)
                      (spaceship-bullets (game-spaceship g)))         ; ListOfBullet
                    empty)]          ; ListofEnemy
        [(key=? key " ") 
         (make-game (make-spaceship 
                      (get_spaceship_pos g) 
                      (add_bullet g)) 
                    empty)]
        [else g]))

;; SpaceInvaders -> Natural
;; produce the current x coordinate of the Spaceship
(check-expect (get_spaceship_pos G0) CENTER_X)

(define (get_spaceship_pos g)
  (spaceship-x (game-spaceship g)))

;; Natural KeyEvent -> Boolean
;; produce true if current x could still go futher to its direction
(check-expect (can_go_to_dir 100 "right") true)
(check-expect (can_go_to_dir 100 "left") true)
(check-expect (can_go_to_dir WIDTH "right") false)
(check-expect (can_go_to_dir 100 "left") true)

;(define (can_go_to_dir x key ke) false)     ; stub

(define (can_go_to_dir x key)
  (cond [(key=? key "right") 
         (< (+ x SPACESHIP_SPEED_X) 
            (- WIDTH (/ (image-width SPACESHIP) 2)))]
        [(key=? key "left")
         (> (- x SPACESHIP_SPEED_X) 
            (/ (image-width SPACESHIP) 2))]))


;; SpaceInvaders -> ListOfBullet
;; ListOfBullet Natural-> ListOfBullet
;; add bullets to the (spaceship-bullets) at the current SPACESHIP_POS_X 
(check-expect (add_bullet (make-game (make-spaceship CENTER_X empty) empty))
              (cons (make-bullet CENTER_X BULLET_FIRE_POS_Y) empty))

(define (add_bullet g)
  (cons (make-bullet 
          (get_spaceship_pos g) 
          BULLET_FIRE_POS_Y) 
        (spaceship-bullets (game-spaceship g))))

