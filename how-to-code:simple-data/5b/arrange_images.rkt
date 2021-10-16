(require 2htdp/image)

(define IMAGE0 (square 1 "solid" "white"))
(define IMAGE1 (square 2 "solid" "white"))
(define IMAGE2 (square 3 "solid" "white"))
(define IMAGE3 (square 4 "solid" "white"))

;; ListOfImages is one of:
;; - empty
;; - (cons Image ListOfImages)
;; interp. list of images
(define LOI0 empty)
(define LOI1 (cons IMAGE1 empty))
(define LOI2 (cons IMAGE3 (cons IMAGE1 empty))) 
(define LOI3 (cons IMAGE3 (cons IMAGE1 (cons IMAGE2 empty)))) 

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
          (... (first loi)
               (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImages)
;; - self-reference: (rest loi) is ListOfImages


;; ListOfImages -> Image
;; produces a list that has been arranged in increasing order given an unordered list
(check-expect (arrangeImages LOI2) (beside IMAGE1 IMAGE3 empty-image))
(check-expect (arrangeImages LOI3) (beside IMAGE1 IMAGE2 IMAGE3 empty-image))

;(define (arrangeImages loi) loi) ;stub

(define (arrangeImages loi)
  (renderImage (sortList loi)))


;; ListOfImages -> Image
;; render images beside each other
(check-expect (renderImage LOI0) empty-image)
(check-expect (renderImage LOI2) (beside IMAGE3 IMAGE1 empty-image))

;(define (renderImage loi) empty-image) ; stub

(define (renderImage loi)
  (cond [(empty? loi) empty-image]
        [else
          (beside (first loi)
                  (renderImage (rest loi)))]))

(define IMAGE1 (square 2 "solid" "white"))
(define IMAGE2 (square 3 "solid" "white"))
(define IMAGE3 (square 4 "solid" "white"))

;(check-expect (arrangeImages LOI2) (cons IMAGE1 (cons IMAGE2 (cons IMAGE3 empty)))) ; sort check-expect
