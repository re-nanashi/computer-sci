(require 2htdp/image)

;; ==================
;; Data definitions:

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. list of images
(define ImageList0 empty)
(define ImageList1 (cons (square 4 "solid" "red") empty))
(define ImageList2 (cons (square 4 "solid" "red") (cons (rectangle 4 2 "solid" "red") empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else 
          (... (first loi)
               (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - compound: (cons Image ListOfImage)
;; - self-reference: (rest loi) is ListOfImage

;; ==================
;; Functions:

;; ListOfImage -> Natural
;; produces the sum of the area of all the images inside the list
(check-expect (sumOfAllAreas ImageList0) 0)
(check-expect (sumOfAllAreas ImageList1) (+ (* 4 4) 0))
(check-expect (sumOfAllAreas ImageList2) (+ (* 4 4) (+ (* 4 2) 0)))

;(define (sumOfAllAreas loi) 0) ;stub

(define (sumOfAllAreas loi)
  (cond [(empty? loi) 0]
        [else 
          (+ (getArea (first loi))
             (sumOfAllAreas (rest loi)))]))

;; Image -> Number
;; produces the area of the given image
(check-expect (getArea (square 4 "solid" "red")) 16)
(check-expect (getArea (rectangle 4 2 "solid" "red")) 8)

; (define (getArea img) 0) ;stub
#; 
(define (getArea img)
  (... img))

(define (getArea img)
  (* (image-height img) 
     (image-width img)))

