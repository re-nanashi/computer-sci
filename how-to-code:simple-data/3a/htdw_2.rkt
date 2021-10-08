(require 2htdp/image)
(require 2htdp/universe)

;; A simple animated traffic light.

;; ===============
;; Constants:

(define RADIUS 20) ; of each light
(define SPACING 6  ; space between and beside light

(define BACKGROUND (rectangle (+ (* 2 SPACING) (* 2 RADIUS))
                              (+ (* 4 SPACING) (* 6 RADIUS))
                              "solid"
                              "black"))

(define SPACE (square SPACING "solid" "black"))

(define RON 
  (overlay (above 
             SPACE
             (circle RADIUS "solid" "red")
             SPACE
             (circle RADIUS "outline" "yellow")
             SPACE
             (circle RADIUS "outline" "green")
             SPACE)
           BACKGROUND))

(define YON 
  (overlay (above 
             SPACE
             (circle RADIUS "outline" "red")
             SPACE
             (circle RADIUS "solid" "yellow")
             SPACE
             (circle RADIUS "outline" "green")
             SPACE)
           BACKGROUND))

(define GON 
  (overlay (above 
             SPACE
             (circle RADIUS "outline" "red")
             SPACE
             (circle RADIUS "outline" "yellow")
             SPACE
             (circle RADIUS "solid" "green")
             SPACE)
           BACKGROUND))

;; ===============
;; Data definitions:

;; Light is one of:
;; - "red"
;; - "yellow"
;; - "green"
;; interp. the current color of the light
;; <examples are redundant for enumerations>

#;
(define (lightFunc l)
  (cond [(string=? l "red") (...)]
        [(string=? l "yellow") (...)]
        [(string=? l "green") (...)]))

;; Template rules used:
;; one of: 3 cases
;; atomic distinct: "red"
;; atomic distinct: "yellow"
;; atomic distinct: "green"

;; ===============
;; Functions:

;; Light -> Light
;; called to run the animation; start with (main "red")
;; no tests for main function
(define (main l)
  (big-bang l 
            (on-tick next_color 1)          ; Light -> Light
            (to-draw render_light)))        ; Light -> Image


;; Light -> Light
;; produce next color of light
(check-expect (next_color "red") "green")
(check-expect (next_color "yellow") "red")
(check-expect (next_color "green") "yellow")

#;
(define (next_color l)          ; stub
  "red")
; <template from Light>

(define (next_color l)
  (cond [(string=? l "red") "green"]
        [(string=? l "yellow") "red"]
        [(string=? l "green") "yellow"]))

;; Light -> Image
;; produce appropriate image for light color
(check-expect (render_light "red") RON)
(check-expect (render_light "yellow") YON)
(check-expect (render_light "green") GON)

#;
(define (render_light l)
  BACKGROUND)
; <template from Light>

(define (render_light l)
  (cond [(string=? l "red") RON]
        [(string=? l "yellow") YON]
        [(string=? l "green") GON]))

