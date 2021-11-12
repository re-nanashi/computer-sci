(require 2htdp/universe)
(require 2htdp/image)

;; ===============
;; Constants:

(define HEIGHT 300)
(define WIDTH 600)

(define CUTOFF 3)
(define BAR_HEIGHT 20)
(define SPACE (/ BAR_HEIGHT 2))


;; ===================
;; Data definitions:

;; WorldState is a Number
;; interp. x-coordinate of mouse
(define WS0 0)
(define WS1 100)

#;
(define (fn-for-ws ws)
  (... ws))

;; ===================
;; Functions:

;; WorldState -> WorldState
;; start the world with (main 0)
(define (main ws)
  (big-bang ws                              ; WorldState
            (on-mouse mouse_handler)        ; WorldState Integer Integer MouseEvent -> WorldState
            (to-draw  render)))             ; WorldState -> Image

;; WorldState Integer Integer MouseEvent -> WorldState
;; produce the next WorldState by getting the latest mouse x-coordinate.
(check-expect (mouse_handler 0 100 10 "enter") 0)
(check-expect (mouse_handler 100 200 10 "move") 200)
(check-expect (mouse_handler 200 0 0 "leave") 200)

;(define (mouse_handler ws x y me) 0)    ; stub
; <template from MouseEvent>
(define (mouse_handler ws x y me)
  (cond [(mouse=? me "move") x]
        [else ws]))

;; WorldState -> Image
;; render the the Image on to the screen with WorldState as a parameter

(define (render ws)
  (cantor WIDTH (/ ws WIDTH)))

;; Number -> Image
;; given a width(number) produce a cantor set of the given width.
(check-expect (cantor CUTOFF 0) (rectangle CUTOFF BAR_HEIGHT "solid" "blue"))
(check-expect (cantor 32 1)
              (above (rectangle 32 BAR_HEIGHT "solid" "blue")
                     (rectangle 32 SPACE "solid" "white")
                     (rectangle 32 BAR_HEIGHT "solid" "white")))

;(define (cantor w) (empty-scene 0 0))  ; stub

;(define (genrec-fn d)                          ; template
;  (cond [(trivial? d) (trivial-answer d)]
;        [else
;         (... d 
;              (genrec-fn (next-problem d)))]))

(define (cantor w r)
  (cond [(<= w CUTOFF) (rectangle w BAR_HEIGHT "solid" "blue")]
        [else
          (local [(define wc (* w r))
                  (define wr (/ (- w wc) 2))
                  (define ctr (rectangle wc BAR_HEIGHT "solid" "white"))
                  (define side (cantor wr r))]
            (above (rectangle w BAR_HEIGHT "solid" "blue")
                   (rectangle w SPACE "solid" "white")
                   (beside side ctr side)))]))
