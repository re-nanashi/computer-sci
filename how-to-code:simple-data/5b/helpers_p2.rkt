(require 2htdp/image)
(require 2htdp/universe)

;; Make it rain where we want to.

;; =============
;; Constants:

(define WIDTH 300)
(define HEIGHT 300)
(define DROP (ellipse 4 8 "solid" "blue"))

(define CUT_OFF (+ HEIGHT (/ (image-height DROP) 2)))

(define SPEED 1)


(define MTS (rectangle WIDTH HEIGHT "solid" "lightblue"))


;; ==================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Number Number)
;; interp. (make-drop) has 
;;          x as x-screen coordinate
;;          y as y-screen coordinate
(define D1 (make-drop 10 10))
(define D2 (make-drop 20 30))
(define D3 (make-drop 30 150))
(define D4 (make-drop 10 100))

#;
(define (fn-for-d d)
  (... (drop-x d)
       (drop-y d)))

;; Template rules used:
;; - compound: 2 fields

;; ListOfDrop is one of:
;; - empty
;; - (cons Drop ListOfDrop)
;; interp. a list of drops 

(define LOD0 empty)
(define LOD1 (cons D1 empty))
(define LOD2 (cons D1 (cons D2 empty)))
(define LOD3 (cons D3 (cons D2 (cons D1 empty))))
(define LOD4 (cons D2 (cons D1 (cons D4 (cons D3 empty)))))

#; 
(define (fn-for-lod lod)
  (cond [(empty? lod) empty]
        [else
          (... (first lod)
               (fn-for-lod (rest lod)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self-reference: (rest lod) is ListOfDrop

;; ==================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)

(define (main d)
  (big-bang d                           ; ListOfDrop 
           (state true)
           (on-mouse addDrop)           ; ListOfDrop Integer Integer MouseEvent-> ListOfDrop
           (on-tick advanceDrop)        ; ListOfDrop -> ListOfDrop
           (to-draw renderDrop)))       ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent-> ListOfDrop
;; add snow droplets to the screen at the give mouse coordinates on MTS
(check-expect (addDrop empty 5 4 "button-down") (cons (make-drop 5 4) empty))
(check-expect (addDrop empty 5 4 "move") empty)

;(define (addDrop lod x y me) empty) ; stub
; <template from MouseEvent>

(define (addDrop lod x y me)
  (cond [(and (mouse=? me "button-down") (< x WIDTH) (< y HEIGHT))
         (cons (make-drop x y) lod)]
        (else lod)))

;; ListOfDrop -> ListOfDrop
;; advances all y coordinates of the Drops inside the list by SPEED
;; !!! remove drop from list if greater than widht
(check-expect (advanceDrop LOD0) empty)
(check-expect (advanceDrop LOD1) (cons (advanceDroplet D1) empty))
(check-expect (advanceDrop LOD2) (cons (advanceDroplet D1) (cons (advanceDroplet D2) empty)))
(check-expect (advanceDrop LOD3) (cons (advanceDroplet D3) (cons (advanceDroplet D2) (cons (advanceDroplet D1) empty))))

;(define (advanceDrop lod) lod)      ; stub
; <template from ListOfDrop>

(define (advanceDrop lod)
  (cond [(empty? lod) empty]
        [(hasToBeDeleted? lod) 
         (deleteADrop lod)]
        [else
          (cons (advanceDroplet (first lod))
                (advanceDrop (rest lod)))]))

;; ListOfDrop -> Boolean
;; produce true if a drop on the list is subject to deletion 
;; by checking if it's y-coordinate is greater than CUT_OFF
(check-expect (hasToBeDeleted? empty) false)
(check-expect (hasToBeDeleted? (cons (make-drop 10 20) empty)) false)
(check-expect (hasToBeDeleted? (cons (make-drop 10 (+ 1 CUT_OFF)) empty)) true)
(check-expect (hasToBeDeleted? (cons (make-drop 10 20) (cons (make-drop 10 (+ 1 CUT_OFF)) empty))) true)

;(define (hasToBeDeleted? lod) empty) ;stub

(define (hasToBeDeleted? lod)
  (cond [(empty? lod) false]
        [else
          (or  (delete? (first lod))
               (hasToBeDeleted? (rest lod)))]))

;; Drop -> Boolean
;; produce true if given's y-coordinate is greater than cutoff

(define (delete? d)
  (> (+ (drop-y d) SPEED) CUT_OFF))


;; ListOfDrop -> ListOfDrop
;; delete a drop from the list if a drop is subject to deletion
(check-expect (deleteADrop empty) empty)
(check-expect (deleteADrop (cons (make-drop 10 20) empty)) (cons (make-drop 10 20) empty))
(check-expect (deleteADrop (cons (make-drop 10 (+ 1 CUT_OFF)) empty)) empty)
(check-expect (deleteADrop (cons (make-drop 10 20) (cons (make-drop 10 (+ 1 CUT_OFF)) empty))) 
            (cons (make-drop 10 20) empty))

;(define (deleteADrop lod) lod)      ;stub
; <template from ListOfDrop>

(define (deleteADrop lod)
  (cond [(empty? lod) empty]
        [else
          (cond [(delete? (first lod)) 
                 (deleteADrop (rest lod))]
                [else
                  (cons (first lod) 
                        (deleteADrop (rest lod)))])]))


;; Drop -> Drop
;; advances Drop by adding speed to the y coordinate
(check-expect (advanceDroplet D1) (make-drop 10 (+ SPEED 10)))
(check-expect (advanceDroplet D2) (make-drop 20 (+ SPEED 30)))
(check-expect (advanceDroplet D3) (make-drop 30 (+ SPEED 150)))
(check-expect (advanceDroplet D4) (make-drop 10 (+ SPEED 100)))

;(define (advanceDroplet d) d)       ; stub
#;
(define (fn-for-d d)
  (... (drop-x d)
       (drop-y d)))

(define (advanceDroplet d)
  (cond [(empty? d) empty]
        [else
          (make-drop 
            (drop-x d) 
            (+ SPEED (drop-y d)))]))


;; ListOfDrop -> Image
;; render all Drops in the list into the MTS
(check-expect (renderDrop empty) MTS) 
(check-expect (renderDrop LOD1)  (place-image DROP (drop-x D1) (drop-y D1) MTS))
(check-expect (renderDrop LOD2)  (place-image DROP (drop-x D1) (drop-y D1) 
                                              (place-image DROP (drop-x D2) (drop-y D2) MTS)))

;(define (renderDrop lod) MTS)   ; stub
; <template from ListOfDrop>

(define (renderDrop lod)
  (cond [(empty? lod) MTS]
        [else
          (renderDroplet (first lod)
                         (renderDrop (rest lod)))]))

;; Drop Image -> Image
;; render drop into an Image and place it on a given Image
(check-expect (renderDroplet D1 MTS) (place-image DROP (drop-x D1) (drop-y D1) MTS))
(check-expect (renderDroplet D2 MTS) (place-image DROP (drop-x D2) (drop-y D2) MTS))

;(define (renderDroplet d) MTS) ;stub
; <template from Drop>

(define (renderDroplet d img)
  (place-image DROP (drop-x d) (drop-y d) img))

