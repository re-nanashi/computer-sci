(require 2htdp/image)
(require 2htdp/universe)

;; Countdown program that starts at ten, and decreases by one each clock tick 
;; until zero and stays there

;; ===============
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define CENTER_X (/ WIDTH 2))
(define CENTER_Y (/ HEIGHT 2))

(define RATE 1)

(define MTS (empty-scene WIDTH HEIGHT))

;; ===============
;; Data definitions:

;; Countdown is a Integer[0,10]
;; interp. the numbers of seconds in a countdown to 0
(define countdown1 10)       ; start of count 
(define countdown2 5)        ; middle of count 
(define countdown3 0)        ; end of count 

#;
(define (countdownFunc c)
  (... c))

;; Template rules used:
;; - atomic non-distinct: Integer[0,10]

;; ===============
;; Functions:

;; Countdown -> Countdown
;; start the world with (main 0)
(define (main c)
  (big-bang c                                    ; Countdown
            (on-tick advance_countdown RATE)     ; Countdown Speed-> Countdown
            (to-draw render)                     ; Countdown -> Image
            (on-key handle_key)))                ; Countdown KeyEvent -> Countdown    

;; Countdown -> Countdown
;; produce the count by subtracting 1 to countdown
(check-expect (advance_countdown countdown1) 9)
(check-expect (advance_countdown countdown2) 4)
(check-expect (advance_countdown countdown3) 0)

;(define (advance_countdown c) 0)        ; stub
; <use template from Countdown definition>

(define (advance_countdown c)
  (cond [(equal? s 0) 0]
        [else (- c 1)]))

;; Countdown -> Image
;; render number image black at the center on MTS while Countdown > 3, red if not
(check-expect (render 5) (createNumberImage 5 "black"))
(check-expect (render 3) (createNumberImage 3 "red"))

;(define (render c) MTS)                    ; stub
; <use template from Countdown definition>

(define (render c)
  (cond [(> c 3) (createNumberImage c "black")]
        [else (createNumberImage c "red")]))

;; render::helper func: Countdown Color-> Image
;; creates a number image 
(check-expect (createNumberImage 10 "black") (place-image (text (number->string 10) 50 "black") CENTER_X CENTER_Y MTS))

;(define (createNumberImage c color) MTS)    ; stub
; <use template from Countdown definition>

(define (createNumberImage c color)
  (place-image (text (number->string c) 50 color) CENTER_X CENTER_Y MTS))

; Countdown KeyEvent -> Countdown    
;; reset countdown when spacebar is pressed
(check-expect (handle_key 10 " ") 10)
(check-expect (handle_key 10 "a") 10)
(check-expect (handle_key 5 " ") 10)
(check-expect (handle_key 5 "a") 5)

;(define (handle_key c key) 0)      ; stub

(define (handle_key c key)
  (cond [(key=? key " ") 10]
        [else c]))
