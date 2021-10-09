(require 2htdp/image)
(require 2htdp/universe)

;; Rotating red square program  

;; =================
;; Constants:

(define HEIGHT 700)
(define WIDTH HEIGHT)

(define CENTER_X (/ WIDTH 2))
(define CENTER_Y (/ HEIGHT 2))

(define COLOR "red")

(define ROTATION_SPEED 20)
(define EXPANSION_SPEED 5)

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

(define-struct box (len ro))
;; Square is (make-box Natural Natural)
;; interp. (make-box) is a square with
;;            len is the square's side length
;;            ro is the square's rotation angle 
(define square1 (make-box 10 20))
(define square2 (make-box 50 120))

#;
(define (squareFunc s)
  (... (box-len s)           ; Natural
       (box-ro s)))          ; Natural
;; Template rules used:
;; - Compound: 2 fields

;; =================
;; Functions:

;; Box -> Box
;; start the world with (main (make-box 0 0))
;; 
(define (main s)
  (big-bang s                          ; Box
            (on-tick   next_box)       ; Box -> Box
            (to-draw   render)         ; Box -> Image
            (on-key    reset_box)))    ; Box KeyEvent -> Box

;; Box -> Box
;; produce the next Square by EXPANSION_SPEED and ROTATION_SPEED
(check-expect (next_box (make-box 10 50)) 
              (make-box (+ 10 EXPANSION_SPEED) (+ 50 ROTATION_SPEED)))

;(define (next_box s) s)        ; stub
; <template from box>

(define (next_box s)
  (make-box (+ (box-len s) EXPANSION_SPEED) 
            (+ (box-ro s) ROTATION_SPEED)))


;; Box -> Image
;; render square according to the current big-bang state at the CENTER_X and Y 
(check-expect (render (make-box 70 120))
              (place-image (rotate (remainder 120 360) (square 70 "solid" COLOR)) 
               CENTER_X 
               CENTER_Y 
               MTS))

;(define (render s) MTS)
; <template from box>

(define (render s) 
  (place-image (rotate (remainder (box-ro s) 360) (square (box-len s) "solid" COLOR)) 
               CENTER_X 
               CENTER_Y 
               MTS))

;; Box KeyEvent -> Box
;; reset the world when spacebar is pressed
(check-expect (reset_box (make-box 40 20) " ") (make-box 0 0))
(check-expect (reset_box (make-box 40 20) "a") (make-box 40 20))

;(define (reset_box s key) s)
; <template from key event>

(define (reset_box s key)
  (cond [(key=? key " ") (make-box 0 0)]
        [else s]))

(main (make-box 0 0) )
