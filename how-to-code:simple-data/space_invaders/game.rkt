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

;; ==================
;; Data definitions:

;(define-struct bullet (x y))
;; !!! create a bullet list

;; ListOfBullet is one of:
;; - empty
;; - (cons Bullet ListOfBullet)
(define LOB0 empty)

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
          (... (first lob)
               (fn-for-lob (rest lob)))]))

;; Template rules used: 
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Bullet ListOfBullet)
;; - self-reference: (rest lob) is ListOfBullet

(define-struct spaceship (x bullets))
;; Spaceship is (make-spaceship Natural ListOfBullet)
;; interp. a spaceship at screen coordinate x
;;          bullets is ListOfBullet that is fired by the spaceship
(define SS0 (make-spaceship CENTER_X empty))

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
            ;(on-tick      ...)            ; SpaceInvaders -> SpaceInvaders
            (to-draw    render)            ; SpaceInvaders -> Image
            ;(stop-when  ...)              ; SpaceInvaders -> Boolean
            (on-key     handle_key)))          ; SpaceInvaders KeyEvent -> SpaceInvaders

;; SpaceInvaders -> Image
;; render an image according to (spaceship-x (game-spaceship g))
;; !!! render all images from Spaceship to Enemies
(check-expect (render G0) (place-image SPACESHIP CENTER_X SPACESHIP_POS_Y MTS)) 

;(define (render g) empty-scene)     ; stub
; <template from SpaceInvaders>

;; !!! render only spaceship for now
;(define (render g)
;  (... (game-spaceship g)               
;       (game-enemies g)))              

(define (render g)
  (place-image 
    SPACESHIP (spaceship-x (game-spaceship g)) SPACESHIP_POS_Y MTS))


;; SpaceInvaders KeyEvent -> SpaceInvaders
;; move Spaceship by adding or subtract SPACESHIP_SPEED_X to/from current x
;; !!! add argument for lists and spacebar
(check-expect (handle_key G0 "right") 
              (make-game (make-spaceship (+ CENTER_X SPACESHIP_SPEED_X) empty) empty))

(check-expect (handle_key G0 "left") 
              (make-game (make-spaceship (- CENTER_X SPACESHIP_SPEED_X) empty) empty))

;(define (handle_key g key) g)       ; stub
; <template from KeyEvent>

(define (handle_key g key)
  (cond [(and (key=? key "right") 
              (< (+ (spaceship-x (game-spaceship g)) SPACESHIP_SPEED_X) 
                 (- WIDTH (/ (image-width SPACESHIP) 2)))) 
         (make-game (make-spaceship 
                      (+ (spaceship-x (game-spaceship g)) SPACESHIP_SPEED_X)
                      empty)         ; ListOfBullet
                    empty)]          ; ListofEnemy
        [(and (key=? key "left") 
              (> (- (spaceship-x (game-spaceship g)) SPACESHIP_SPEED_X) 
                 (/ (image-width SPACESHIP) 2 )))
         (make-game (make-spaceship 
                      (- (spaceship-x (game-spaceship g)) SPACESHIP_SPEED_X)
                      empty)         ; ListOfBullet
                    empty)]          ; ListofEnemy
        [else 
         g]))

