(require 2htdp/image)
(require 2htdp/universe)

;; MouseCursor Tracker program  (make this more specific)

;; =================
;; Constants:
(define HEIGHT 500)
(define WIDTH HEIGHT)
(define MTS (empty-scene WIDTH HEIGHT))

(define CENTER_X (/ WIDTH 2))
(define CENTER_Y (/ HEIGHT 2))

;; =================
;; Data definitions:

(define-struct cursor (x y))
;; Cursor is (make-cursor Natural Natural)
;; interp. (make-cursor) is a cursor with
;;          x is the cursor position at x coordinate            
;;          y is the cursor position at y coordinate            
(define cursor1 (make-cursor 100 20))
(define cursor2 (make-cursor 20 100))

#;
(define (cursorFunc c)
  (... (cursor-x c)             ; Natural
       (cursor-y c)))           ; Natural

;; Template rules used:
;; - Compound: 2 fields

;; =================
;; Functions:

;; Cursor -> Cursor
;; start the world with (main (make-cursor 0 0))
;; 
(define (main c)
  (big-bang c                       ; Cursor
            (to-draw   render)      ; Cursor -> Cursor
            (on-mouse  update)))    ; Cursor Integer Integer MouseEvent -> Cursor
               

;; Cursor -> Cursor
;; update the mouse position
(check-expect (update (make-cursor 0 0) 100 10 "move") (make-cursor 100 10))
(check-expect (update (make-cursor 0 0) 100 10 "enter") (make-cursor 100 10))
(check-expect (update (make-cursor 100 20) 100 20 "leave") (make-cursor 0 0))

;(define (update c x y me) c)   ; stub
; <template from Cursor>

(define (update c x y me)
  (cond [(mouse=? me "enter") (make-cursor x y)]
        [(mouse=? me "move") (make-cursor x y)]
        [(mouse=? me "leave") (make-cursor 0 0)]))


;; Cursor -> Image
;; render the current mouse position 
(define (render c) 
  (place-image 
    (text (string-append "(" (number->string (cursor-x c)) "," (number->string (cursor-y c)) ")") 50 "black")
    CENTER_X
    CENTER_Y
    MTS))
