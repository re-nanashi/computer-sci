(require 2htdp/image)
(require 2htdp/universe)

;; Growing grass program

;; =================
;; Constants:

(define HEIGHT 500)
(define WIDTH (* 500 2))
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "blue"))
(define BACKGROUND_BOTTOM HEIGHT)
(define SPACE 20)

(define PLANT_COLOR "green")
(define PLANT_WIDTH 20)
(define GROWTH_SPEED 5)

;; =================
;; Data definitions:

(define-struct grass (x y))
;; Grass is (make-grass Natural Natural)
;; interp. (make-grass) is a plant/grass with
;;          x is the current position of the grass relative to width
;;          y is the height of the grass
(define grass1 (make-grass 20 150))
(define grass2 (make-grass 0 150))

#;
(define (grassFunc s)
  (... (grass-x s)          ; Natural
       (grass-y s)))        ; Natural
;; Template rules used:
;; - Compound: 2 fields

;; =================
;; Functions:

;; Grass -> Grass
;; Start the world with (main (make-grass SPACE 0))
;; !!!

(define (main g)
  (big-bang g                                ; Grass
            (on-tick   increaseHeight)       ; Grass -> Grass
            (to-draw   render)               ; Grass -> Image
            (on-key    advanceGrassPos)))    ; Grass KeyEvent -> Grass

;; Grass -> Grass
;; increase grass height by adding GROWTH_SPEED to y
(check-expect (increaseHeight (make-grass 20 100))
              (make-grass 20 (+ 100 GROWTH_SPEED)))

;(define (increaseHeight g) g)       ; stub
; <template from grass>

(define (increaseHeight g)
  (make-grass (grass-x g) (+ (grass-y g) GROWTH_SPEED)))

;; Grass -> Image
;; render the grass to the current x
(check-expect (render (make-grass 20 100))
              (place-image (rectangle PLANT_WIDTH 100 "solid" PLANT_COLOR) 
                           20 
                           BACKGROUND_BOTTOM 
                           BACKGROUND))

; (define (render g) BACKGROUND) ; stub
; <template from grass>

(define (render g)
  (place-image (rectangle PLANT_WIDTH (grass-y g) "solid" PLANT_COLOR) 
               (grass-x g) 
               BACKGROUND_BOTTOM 
               BACKGROUND))

;; Grass KeyEvent -> Grass
;; advances grass position and reset y value of grass to zero
(check-expect (advanceGrassPos (make-grass 20 100) " ") (make-grass 40 0))
(check-expect (advanceGrassPos (make-grass 20 100) "a") (make-grass 40 0))

;(define (advanceGrassPos g) g)      ; stub
; <template from key event>
  
(define (advanceGrassPos g key)
  (cond [(key-event? key) (make-box (+ (grass-x g) SPACE) 0)]))

(main (make-grass SPACE 0))
