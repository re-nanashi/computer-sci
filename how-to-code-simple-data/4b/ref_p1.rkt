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

(define-struct school (name tuition next))
;; School is one of:
;;  - false
;;  - (make-school String Natural School)
;; interp. an arbitrary number of schools, where for each school we have its
;;         name and its tuition in USD

(define school0 false)
(define school1 (make-school "ICC" 40000 school0))
(define school2 (make-school "RCI" 30000 school1))

#; 
(define (fn-for-s sch)
  (cond [(false? sch) (...)]
        [else 
          (... (school-name sch) (school-tuition sch)
               (fn-for-s (school-next sch)))]))

;; Template rules used:
;; one of: 2 cases
;; atomic distinct: false
;; compound: (make-school String Natural School)
;; self-reference: (school-next sch) is School

;; ==========
;; Functions:

;; ListOfSchools -> Image
;; render a Bar image of the school's tuition with it's name, besides each other
(check-expect (renderChart school0) (square 0 "solid" "white"))
(check-expect (renderChart school1) (renderBar school1))
(check-expect (renderChart school2) (beside/align "bottom" (renderBar school2) 
                                               (renderBar (school-next school2))))

;(define (renderChart los) (empty-scene BAR_WIDTH 0))   ;stub
; <template from School>

(define (renderChart sch)
  (cond [(false? sch) (square 0 "solid" "white")]
        [else 
          (beside/align "bottom" (renderBar sch)
                        (renderChart (school-next sch)))]))


;; School::Compound -> Image
;; render an Image according to school data 
;; (school-name) as text and (school-tuition) as height
(check-expect (renderBar school1) (overlay/align "center" "bottom" (rotate 90 (text (school-name school1) FONT_SIZE FONT_COLOR))
                                               (rectangle BAR_WIDTH (* (school-tuition school1) Y_SCALE) "outline" "black")
                                               (rectangle BAR_WIDTH (* (school-tuition school1) Y_SCALE) "solid" BAR_COLOR)))

(check-expect (renderBar school2) (overlay/align "center" "bottom" (rotate 90 (text (school-name school2) FONT_SIZE FONT_COLOR))
                                               (rectangle BAR_WIDTH (* (school-tuition school2) Y_SCALE) "outline" "black")
                                               (rectangle BAR_WIDTH (* (school-tuition school2) Y_SCALE) "solid" BAR_COLOR)))

; (define (renderBar s) (square 0 "solid" "white"))    ;stub
#;
(define (fn-for-sc s)
  (... (school-name s)          ; String
       (school-tuition s)))     ; Natural

(define (renderBar s)
  (overlay/align "center" "bottom" (rotate 90 (text (school-name s) FONT_SIZE FONT_COLOR)) 
                 (rectangle BAR_WIDTH (* (school-tuition s) Y_SCALE) "outline" "black") 
                 (rectangle BAR_WIDTH (* (school-tuition s) Y_SCALE) "solid" BAR_COLOR)))         

