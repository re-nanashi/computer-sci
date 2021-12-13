(require 2htdp/image)

;; Tuition analysis program

;; ==========
;; Constants:

(define BAR_WIDTH 30)
(define BAR_CENTER (/ BAR_WIDTH 2))
(define BAR_COLOR "lightblue")

(define FONT_SIZE (- BAR_WIDTH 6))
(define FONT_COLOR "black")

(define Y_SCALE 1/200)

;; ==========
;; Data definitions:

(define-struct school (name tuition))
;; School is (make-school String Natural)
;; interp. (make-school) has a 
;;          name - the school's name 
;;          tuition fee - the school's tuition in PHP

(define school0 (make-school "RCI" 30000))
(define school1 (make-school "ICC" 40000))
(define school2 (make-school "PUP" 1800))

#; 
(define (fn-for-s s)
  (... (school-name s)              ; String
       (school-tuition s)))         ; Natural

;; Template rules used:
;; - Compound: 2 fields (make-school String Natural)

;; ListOfSchool is one of:
;; - empty
;; - (cons School ListOfSchool)
;; interp. a list of schools
(define LOS0 empty)
(define LOS1 (cons school0 empty))
(define LOS2 (cons school0 (cons school1 empty)))
(define LOS3 (cons school0 (cons school2 empty)))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
          (... (fn-for-s (first los))
               (fn-for-los (rest los)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons School ListOfSchool)
;; - reference: (first los) is School
;; - self-reference: (rest los) is ListOfSchool


;; ==========
;; Functions:

;; ListOfSchools -> Image
;; render a Bar image of the school's tuition with it's name besides each other
(check-expect (renderChart LOS0) (square 0 "solid" "white"))
(check-expect (renderChart LOS1) (renderBar (first LOS1)))
(check-expect (renderChart LOS2) (beside/align "bottom" (renderBar (first LOS2)) 
                                               (renderBar (first (rest LOS2)))))

;(define (renderChart los) (empty-scene BAR_WIDTH 0))   ;stub
;; <template from ListOfSchool>
(define (renderChart los)
  (cond [(empty? los) (square 0 "solid" "white")]
        [else
          (beside/align "bottom" (renderBar (first los)) 
                        (renderChart (rest los)))]))

;; School -> Image
;; render an Image according to school data 
;; (school-name) as text and (school-tuition) as height
(check-expect (renderBar school0) (overlay/align "center" "bottom" (rotate 90 (text (school-name school0) FONT_SIZE FONT_COLOR))
                                               (rectangle BAR_WIDTH (* (school-tuition school0) Y_SCALE) "outline" "black")
                                               (rectangle BAR_WIDTH (* (school-tuition school0) Y_SCALE) "solid" BAR_COLOR)))

(check-expect (renderBar school1) (overlay/align "center" "bottom" (rotate 90 (text (school-name school1) FONT_SIZE FONT_COLOR))
                                               (rectangle BAR_WIDTH (* (school-tuition school1) Y_SCALE) "outline" "black")
                                               (rectangle BAR_WIDTH (* (school-tuition school1) Y_SCALE) "solid" BAR_COLOR)))

; (define (renderBar s) (empty-scene BAR_WIDTH 0))    ;stub
; <template from School>

(define (renderBar s)
  (overlay/align "center" "bottom" (rotate 90 (text (school-name s) FONT_SIZE FONT_COLOR)) 
                 (rectangle BAR_WIDTH (* (school-tuition s) Y_SCALE) "outline" "black") 
                 (rectangle BAR_WIDTH (* (school-tuition s) Y_SCALE) "solid" BAR_COLOR)))         

;; ListOfSchool -> School
;; produces the lowest international student tuition
(check-expect (lowestTuition LOS1) (first LOS1))
(check-expect (lowestTuition LOS2) (first LOS2))
(check-expect (lowestTuition LOS3) (first (rest LOS3)))

;(define (lowestTuition los) (make-school "empty" 0)) ;stub
; <template from ListOfSchool>

(define (lowestTuition los)
  (cond [(empty? (rest los)) (first los)]
        [else
          (if (compare (first los) (lowestTuition (rest los))) 
            (first los)
            (lowestTuition (rest los)))]))

(define (compare s1 s2)
  (< (school-tuition s1) (school-tuition s2)))

