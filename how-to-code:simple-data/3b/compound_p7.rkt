(require 2htdp/image)
(require 2htdp/universe)

;; A program that animates a pokeball rolling across the screen

;; ==========
;; Constants: 

(define PI 3.14159)

(define POKEBALL .)
(define RADIUS (/ (image-height POKEBALL) 2))
(define CIRCUMFERENCE (* 2 PI RADIUS))

(define HEIGHT (* RADIUS 3))
(define WIDTH (+ CIRCUMFERENCE (* RADIUS 2)))
(define CENTER_Y (* RADIUS 2))
(define MTS (empty-scene WIDTH HEIGHT))

; Given an angular speed we can compute x speed
(define X_PER_DEGREE (/ CIRCUMFERENCE 360))
; since the rotate function in racket rotate counter-clockwise, negative for clockwise rotation
(define ANGULAR_SPEED -2)
; The distance the pokeball needs to travel in terms of angular speed
; x = angle
; y(x) = X_PER_DEGREE * x
(define X_SPEED (* -1 ANGULAR_SPEED X_PER_DEGREE))

;; ==========
;; Data definitions:

(define-struct PokeBallState (dir x ro))
;; PokeBallState is (make-PokeBallState Number Number[0, WIDTH] Number[0, 360))
;; interp. (make-PokeBallState) is with 
;;          dir is direction of pokeball, -1 to the left and +1 to the right
;;          x is the current x-coordinate of pokeball
;;          ro is the current angular rotation of pokeball
(define PokeBallState1 (make-PokeBallState 1 100 (/ 100 X_PER_DEGREE)))
(define PokeBallState2 (make-PokeBallState -1 100 (/ 100 X_PER_DEGREE)))

#;
(define (PokeBallStateFunc pbs)
  (... (PokeBallState-dir pbs)      ; Number
       (PokeBallState-x pbs)        ; Number[0, WIDTH]
       (PokeBallState-ro pbs)))     ; Number[0, 360) 

;; Template rules used:
;; - Compound: 2 fields

;; ==========
;; Functions:

(define (main pbs)
  (big-bang pbs                     ; PokeBallState
            (on-tick next_pbs)            ; PokeBallState -> PokeBallState
            (to-draw render_pb)           ; PokeBallState -> Image
            (on-mouse change_dir)))       ; PokeBallState Integer Integer MouseEvent-> PokeBallState

;; PokeBallState -> PokeBallState
;; advances the next pokeball state by adding X_SPEED to (PokeBallState-x)
;; !!! - add rotation
(check-expect (next_pbs (make-PokeBallState 1 60 100))
              (make-PokeBallState 1 
                                  (+ 60 X_SPEED)
                                  (+ 100 ANGULAR_SPEED)))

(check-expect (next_pbs (make-PokeBallState 1 (- WIDTH RADIUS) 1))
              (make-PokeBallState -1 
                                  (- WIDTH RADIUS)
                                  1))

(check-expect (next_pbs (make-PokeBallState -1 0 359))
              (make-PokeBallState 1
                                  RADIUS 
                                  359))

(check-expect (next_pbs (make-PokeBallState 1 100 20))
              (make-PokeBallState 1 
                                  (+ 100 X_SPEED)
                                  (+ 20 ANGULAR_SPEED)))

;(define (next_pbs pbs) pbs)     ;stub
; <template from PokeBallState>

(define (next_pbs pbs)
        ; if x + (speed * direction) becomes greater than width, change its direction to the left
  (cond [(>= (+ (PokeBallState-x pbs) (* (PokeBallState-dir pbs) X_SPEED)) (- WIDTH RADIUS))
         (make-PokeBallState -1 (- WIDTH RADIUS) 1)]
        ; if x + (speed * direction) becomes less than (0 + RADIUS), change its direction to right left
        [(<= (+ (PokeBallState-x pbs) (* (PokeBallState-dir pbs) X_SPEED)) RADIUS)
         (make-PokeBallState 1 RADIUS 359)]
        [else 
          (make-PokeBallState (PokeBallState-dir pbs)
                              ; add current x pos by the speed(- if going to the left and + if going to the right)
                              (+ (PokeBallState-x pbs) (* (PokeBallState-dir pbs) X_SPEED))
                              (modulo (+ (PokeBallState-ro pbs) (* (PokeBallState-dir pbs) ANGULAR_SPEED)) 359))]))

;; PokeBallState -> Image
;; render the current state and produce an image at the (PokeBallState-x)
;; !!! - add rotation
(check-expect (render_pb PokeBallState1) (place-image (rotate (PokeBallState-ro PokeBallState1) POKEBALL) 100 CENTER_Y MTS))

;(define (render_pb pbs) MTS) ; stub
; <template from PokeBallState>

(define (render_pb pbs)
  (place-image (rotate (PokeBallState-ro pbs) POKEBALL) 
               (PokeBallState-x pbs) 
               CENTER_Y 
               MTS))

;; PokeBallState Integer Integer MouseEvent-> PokeBallState
;; change PokeBallState dir when mouse is clicked 
;; !!! - add rotation
(check-expect (change_dir PokeBallState1 0 0 "button-down") (make-PokeBallState -1 100 (/ 100 X_PER_DEGREE)))
(check-expect (change_dir PokeBallState2 10 20 "button-down") (make-PokeBallState 1 100 (/ 100 X_PER_DEGREE)))

; (define (change_dir pbs x y me) pbs) ; stub
; <template from MouseEvent>

(define (change_dir pbs x y me)
  (cond [(mouse=? "button-down" me) 
         (make-PokeBallState (* -1 (PokeBallState-dir pbs))
                             (PokeBallState-x pbs)
                             (PokeBallState-ro pbs))]
        [else pbs]))

(main (make-PokeBallState 1 RADIUS 359))
