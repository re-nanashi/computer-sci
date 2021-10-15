(require 2htdp/image)
(require 2htdp/universe)

;; Saiki program that uses his powers to multiply and spin

;; ===============
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)
(define MTS (empty-scene WIDTH HEIGHT))

(define SAIKI_IMG .)
(define ANGULAR_SPEED 3)

;; ===============
;; Data definitions:


(define-struct saiki (x y rotation))
;; Saiki is one of:
;; - empty
;; - (make-saiki Number[0, WIDTH) Number[0, HEIGHT) Number[0, 360))
;; interp. saiki at position x, y at screen coordinates and it's rotation angle
(define saiki0 empty)
(define saiki1 (make-saiki 100 200 250))
(define saiki2 (make-saiki 200 100 180))

#; 
(define (fn-for-s s)
  (cond [(empty? s) (...)]
        [else
          (... (saiki-x s)                   ; Number[0, WIDTH)
               (saiki-y s)                   ; Number[0, HEIGHT)
               (saiki-rotation s))]))        ; Number[0, 360)

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (make-saiki Number[0, WIDTH) Number[0, HEIGHT) Number[0, 360))

;; ListOfSaiki is one of:
;; - empty
;; - (cons Saiki ListOfSaiki)
;; interp. a list of saikis
(define LOS0 empty)
(define LOS1 (cons saiki1 empty))
(define LOS2 (cons saiki1 (cons saiki2 empty)))

#;
(define (fn-for-los los)
  (cond [(empty?) (...)]
        [else
          (... (fn-for-s (first los))
               (fn-for-los (rest los)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Saiki ListOfSaiki)
;; - reference: (first los) is Saiki
;; - self-reference: (rest los) is ListOfSaiki


;; ===============
;; Functions:

;; ListOfSaiki -> ListOfSaiki
;; Start the world with (main empty)
(define (main s)
  (big-bang s                               ; Saiki
            (on-tick rotate_all_saiki)      ; Saiki -> Saiki
            (to-draw render_all)            ; Saiki -> Image
            (on-mouse advance_saiki)))      ; Saiki Integer Integer MouseEvent-> Saiki

;; ListOfSaiki -> ListOfSaiki
;; rotate all saikis on the list
(check-expect (rotate_all_saiki empty) empty)
(check-expect (rotate_all_saiki LOS1) (cons (rotate_saiki (first LOS1)) empty))
(check-expect (rotate_all_saiki LOS2) (cons (rotate_saiki (first LOS2)) (cons (rotate_saiki (first (rest LOS2))) empty)))

;(define (rotate_all_saiki los) los)     ; stub
; <template from ListOfSaiki>

(define (rotate_all_saiki los)
  (cond [(empty? los) empty]
        [else
          (cons (rotate_saiki (first los))
               (rotate_all_saiki (rest los)))]))

;; Saiki -> Saiki
;; rotates the current saiki by adding angular speed
(check-expect (rotate_saiki saiki1) 
              (make-saiki (saiki-x saiki1) 
                          (saiki-y saiki1) 
                          (- (saiki-rotation saiki1) ANGULAR_SPEED)))

(check-expect (rotate_saiki saiki2) 
              (make-saiki (saiki-x saiki2) 
                          (saiki-y saiki2) 
                          (- (saiki-rotation saiki2) ANGULAR_SPEED)))

;(define (rotate_saiki s) s) ; stub
;<template from Saiki>

(define (rotate_saiki s)
  (cond [(empty? s) empty]
        [else
          (make-saiki (saiki-x s) 
              (saiki-y s) 
              (- (saiki-rotation s) ANGULAR_SPEED))]))

;; ListOfSaiki -> Image
;; render all saiki image on the list on their respective coordinates
(check-expect (render_all empty) (place-image (square 0 "solid" "white" ) 0 0 MTS))


;(define (render_all los) MTS)       ;stub
; <template from ListOfSaiki>
(define (render_all los)
  (cond [(empty? los) MTS]
        [else
          (render (first los)
                  (render_all (rest los)))]))

;; Saiki -> Image
;; render saiki image to the given x, y coordinates
;; !!! add rotation

(check-expect (render saiki1 MTS) (place-image (rotate (saiki-rotation saiki1) SAIKI_IMG)
                                           (saiki-x saiki1) 
                                           (saiki-y saiki1) 
                                           MTS))

(check-expect (render saiki2 MTS) (place-image (rotate (saiki-rotation saiki2) SAIKI_IMG)
                                           (saiki-x saiki2) 
                                           (saiki-y saiki2) 
                                           MTS))

;(define (render empty) (square 0 "solid" "white")) ; stub

; <template from Saiki>

(define (render s img)
  (place-image (rotate (saiki-rotation s) SAIKI_IMG) (saiki-x s) (saiki-y s) img))

;; Saiki Integer Integer MouseEvent-> Saiki
;; advance_saiki or moves saiki to the MouseEvent's x and y position on MTS
;; !!!

(check-expect (advance_saiki empty 5 4 "button-down") (cons (make-saiki 5 4 0) empty))
(check-expect (advance_saiki empty 5 4 "move") empty)

;(define (advance_saiki s x y me) empty)     ; stub
; <template from MouseEvent>

(define (advance_saiki s x y me)
  (cond [(and (mouse=? me "button-down") (< x WIDTH) (< y HEIGHT)) 
         (cons (make-saiki x y 0) s)]
        [else s]))
