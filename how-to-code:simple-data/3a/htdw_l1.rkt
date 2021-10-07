(require 2htdp/image)
(require 2htdp/universe)

;; Saiki program that uses his powers to walk all over the screen.

;; ===============
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define CENTER_Y (/HEIGHT 2))

(define SPEED 10)

(define MTS (empty-scene WIDTH HEIGHT))

;; !!!
(define SAIKI_IMG)

;; ===============
;; Data definitions:

;; Saiki is a Number
;; interp. x position of saiki in screen coordinates
(define saiki1 0)           ;left edge 
(define saiki2 (/ WIDTH 2)) ;middle
(define saiki3 WIDTH)       ;right edge 

(define (saikiFunc saiki)
  (... saiki))

;; Template rules used:
;; - atomic non-distinct: Number

;; ===============
;; Functions:

;; Saiki -> Saiki
;; start the world with (main 0)
;;
(define (main s)
  (big-bang s                           ; Saiki  
            (on-tick advance_saiki)     ; Saiki -> Saiki
            (to-draw render)            ; Saiki -> Image 
            (on-key ...)))              ; Saiki KeyEvent -> Saiki  

;; Saiki -> Saiki
;; produce the saiki on the next pos by SPEED pixel to the right
(check-expect (advance_saiki 3) (+ 3 SPEED))

;(define (advance_saiki s) 0)            ; stub
; <use template from Saiki definition>

(define (advance_saiki s)
  (+ s SPEED))

;; Saiki -> Image
;; render saiki image at the appropriate place on MTS
(check-expect (render 4) (place-image SAIKI_IMG 4 CENTER_Y MTS))

;(define (render s) MTS)                 ; stub
; <use template from Saiki definition>

(define (render s)
  (place-image SAIKI_IMG s CENTER_Y MTS))

;; Saiki KeyEvent -> Saiki  
;; reset saiki to left edge when space key is pressed
(check-expect (handle_key 10 " ") 0)
(check-expect (handle_key 10 "a") 0)
(check-expect (handle_key 0 " ") 0)
(check-expect (handle_key 0 "a") 0)

;(define (handle_key s key) 0)            ; stub

(define (handle_key s key)
  (cond [(key=? ke " ") 0]
        [else s]))
