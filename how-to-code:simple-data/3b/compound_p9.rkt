(require 2htdp/image)
(require 2htdp/universe)

;; Spinning pokeball 

;; ==========
;; CONSTANTS

(define WIDTH 600)
(define HEIGHT 300)

(define CENTER_Y (/ HEIGHT 2))

(define LINEAR-SPEED 2)
(define ANGULAR-SPEED 3)

(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

(define POKEBALL)

;; ==========
;; Data Definitions:

(define-struct pokeball (x rotation))
;; PokeBall is (make-pokeball Number Number)
;; interp. The state of a tossed pokeball
;;          x is the x coordinate in pixels
;;          a is the angle of rotation in degrees
(define pokeball1 (make-pokeball 10 0))
(define pokeball2 (make-pokeball 30 15))

#;
(define (pokeballFunc pb) 
  (... (pokeball-x pb)
       (pokeball-rotation pb)))

;; Template rules used:
;; - compound: 2 fields


;; ==========
;; Functions

;; PokeBall -> PokeBall
;; run the animation, starting with initial pokeball state
;; Start with (main (make-pokeball 0 0))

(define (main pb)
  (big-bang pb 
            (on-tick next_pokeball)
            (to-draw render_pokeball)
            (on-key reset_pokeball)))

;; PokeBall -> PokeBall
;; advance pokeball by LINEAR-SPEED and ANGULAR-SPEED
(check-expect (next_pokeball (make-pokeball 1 12)
                             (make-pokeball (+ 1 LINEAR-SPEED) (- 12 ANGULAR-SPEED))))

; (define (next_pokeball pb) pb)      ; stub
; <template from pokeball state>

(define (next_pokeball pb)
  (make-pokeball (+ (pokeball-x pb) LINEAR-SPEED)
                 (+ (pokeball-rotation pb) ANGULAR-SPEED)))

;; PokeBall -> Image
;; produces the pokeball at height pokeball-x rotated (remainder pokeball-rotation 360) on MTS
(check-expect (render_pokeball (make-pokeball 1 12))
              (place-image (rotate 12 WATER-BALLOON)
                           1
                           CTR-Y
                           MTS))
(check-expect (render_pokeball (make-pokeball 10 361))
              (place-image (rotate 1 WATER-BALLOON)
                           10
                           CTR-Y
                           MTS))

; (define (render_pokeball pb) MTS)
; <template from PokeBall>

(define (render_pokeball pb)
  (place-image (rotate (modulo (pokeball-rotation pb) 360) POKEBALL)
               (pokeball-x pb)
               CENTER_Y
               MTS))

;; PokeBall KeyEvent -> PokeBall
;; reset the program 
(check-expect (reset_pokeball (make-pokeball 1 12) " ")
              (make-pokeball 0 0))
(check-expect (reset_pokeball (make-pokeball 1 12) "left")
              (make-pokeball 1 12))

; (define (reset_pokeball pb key) pb) ; stub
; <template from KeyEvent>
(define (reset_pokeball pb key)
  (cond [(key=? " " key) (make-pokeball 0 0)]
        [else pb]))
