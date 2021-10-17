;; ====================
;; Constants:

;; Image
;; interp. an image
(define IMAGE1 (rectangle 3 4 "solid" "black"))
(define IMAGE2 (rectangle 6 2 "solid" "pink"))
(define IMAGE3 (rectangle 5 4 "solid" "red"))
(define IMAGE4 (rectangle 5 5 "solid" "blue"))

#; 
(define (fn-for-image img)
  (... img))

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. list of images
(define LOI0 empty)
(define LOI1 (cons IMAGE1 empty))
(define LOI2 (cons IMAGE2 (cons IMAGE1 empty)))
(define LOI3 (cons IMAGE4 (cons IMAGE3 (cons IMAGE1 empty))))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
          (... (first loi)
               (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Image ListOfImage)
;; - reference: (first loi) is Image
;; - self-reference (rest loi) is ListOfImage

;; ====================
;; Functions:

;; arrangeImages( ListOfImage ) -> Image
;; produce an image that lays out a list in sorted order (increasing)
(check-expect (arrangeImages LOI0) empty-image)
(check-expect (arrangeImages LOI1) (beside IMAGE1 empty-image))
(check-expect (arrangeImages LOI2) (beside IMAGE2 IMAGE1 empty-image))
(check-expect (arrangeImages LOI3) (beside IMAGE1 IMAGE3 IMAGE4 empty-image))

;(define (arrangeImages loi) empty-image) ;stub

(define (arrangeImages loi)
  (renderImages (sortList loi)))

;; renderImages( ListOfImage ) -> Image
;; render the list to images given a sorted list
(check-expect (renderImages LOI0) empty-image) 
(check-expect (renderImages LOI1) (beside IMAGE1 empty-image)) 
(check-expect (renderImages LOI2) (beside IMAGE2 IMAGE1 empty-image)) 

;(define (renderImages loi) empty-image) ;stub
; <template from ListOfImage>

(define (renderImages loi)
  (cond [(empty? loi) empty-image]
        [else
          (beside (first loi)
                  (renderImages (rest loi)))]))

;; sortList( ListOfImage ) -> ListOfImage
;; sort the given list into increasing order
(check-expect (sortList empty) empty)
(check-expect (sortList LOI1) (cons IMAGE1 empty))
(check-expect (sortList LOI2) (cons IMAGE2 (cons IMAGE1 empty)))
(check-expect (sortList LOI3) (cons IMAGE1 (cons IMAGE3 (cons IMAGE4 empty))))

;(define (sortList loi) loi) ; stub
; <template from ListOfImage>

(define (sortList loi)
  (cond [(empty? loi) empty]
        [else
          (insert (first loi)
                  (sortList (rest loi)))]))  ; list will be sorted

;; insert( IMAGE ListOfImage ) -> ListOfImage
;; insert the an image into a list by increasing order
;; !!! ASSUME: that ListOfImage is already sorted
(check-expect (insert IMAGE1 LOI0) LOI1)
(check-expect (insert IMAGE2 LOI1) (cons IMAGE2 (cons IMAGE1 empty)))
(check-expect (insert IMAGE3 (cons IMAGE2 (cons IMAGE1 empty))) (cons IMAGE2 (cons IMAGE1 (cons IMAGE3 empty))))

;(define (insert img loi) loi) ;stub

(define (insert img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
          (if (larger? img (first loi))
            ;; if img is larger put first in front of the list and recurse until sorted
            (cons (first loi) (insert img (rest loi)))
            ;; if (first loi) is larger, put img in front of the list
            (cons img loi))]))

;; larger? (Image Image) -> Boolean
;; produce true if Image1 is larger than Image2
(check-expect (larger? IMAGE2 IMAGE1) false)
(check-expect (larger? IMAGE1 IMAGE3) false)
(check-expect (larger? IMAGE4 IMAGE3) true)


(define (larger? img1 img2)
  (> (* (image-width img1) (image-height img1)) 
     (* (image-width img2) (image-height img2))))

