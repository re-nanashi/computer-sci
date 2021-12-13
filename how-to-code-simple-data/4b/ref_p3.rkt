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


;; ListOfSchoolNames is one of:
;; - empty
;; - (cons String ListOfSchoolNames)
;; interp. a list of school names
(define LOSN0 empty)
(define LOSN1 (cons (school-name school0) empty))
(define LOSN2 (cons (school-name school1) (cons (school-name school2) empty)))
(define LOSN3 (cons (school-name school0) (cons (school-name school1) (cons (school-name school2) empty))))

#; 
(define (fn-for-losn losn)
  (cond [(empty? losn) (...)]
        [else
          (... (fn-for-s (first losn))
               (fn-for-losn (rest losn)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons String ListOfSchoolNames)
;; - reference: (first losn) is String
;; - self-reference: (rest losn) is ListOfSchoolNames<string>

;; ==========
;; Functions:

;; ListOfSchool -> School
;; produce the lowest international student tuition
(check-expect (lowestTuition LOS1) (first LOS1))
(check-expect (lowestTuition LOS2) (first LOS2))
(check-expect (lowestTuition LOS3) (first (rest LOS3)))

;(define (lowestTuition los) (make-school "empty" 0)) ; stub
; <template from ListOfSchool>
#;
(define (fn-for-nonEmpty nonEmpty)
  (cond [(empty? (rest nonEmpty)) (...  (first nonEmpty))] 
        [else
          (... (first nonEmpty) 
               (fn-for-nonEmpty (rest nonEmpty)))]))

(define (lowestTuition los)
  (cond [(empty? (rest los)) (first los)]
        [else
          (if (compare (first los) (lowestTuition (rest los))))
          (first los)
          (lowestTuition (rest los))]))

;; School School -> Boolean
;; compare two schools and produce true if first parameter is less than second
(check-expect (compare school0 school1) true)
(check-expect (compare school1 school2) false)


;(define (compare s1 s2) false)  ;stub
#;
(define (compare s1 s2)         ; template
  (... (school-name s1)
       (school-tuition s1)
       (school-name s2)
       (school-tuition s2)))

(define (compare s1 s2)
  (< (school-tuition s1) (school-tuition s2)))

;; ListOfSchool -> ListOfSchoolNames
;; produce a list of school names given a list of school
;(check-expect (getNames LOSN0) 0) ; atleast one 
(check-expect (getNames LOS1) (cons "RCI" empty))
(check-expect (getNames LOS2) (cons "RCI" (cons "ICC" empty)))
(check-expect (getNames LOS3) (cons "RCI" (cons "PUP" empty)))

;(define (getNames los) empty)  ;stub
;<template from ListOfSchool>

(define (getNames los)
  (cond [(empty? los) empty]
        [else
          (cons (school-name (first los))
               (getNames (rest los)))]))
