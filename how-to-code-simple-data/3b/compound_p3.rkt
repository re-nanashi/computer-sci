;; ================
;; Data definitions:

(define-struct student (name grade hasAllergies))
;; Student is (make-student String Number[1,12], Boolean)
;; interp. a (make-student) is a student with 
;;           name is the student's name
;;           grade is the student's grade
;;           hasAllergies is a var if a student has allergies
(define student1 (make-student "Jose" 12 true))
(define student2 (make-student "John" 12 false))

(define (studentFunc s)
  (... (student-name s)             ; String
       (student-grade s)            ; Number[1,12]
       (student-hasAllergies s)))   ; Boolean

;; Template rules used:
;; - Compound: 3 fields

;; ================
;; Functions:

;; Student -> Boolean
;; produces true if student has allergies
(check-expect (checkIfStudentHaveAllergies student1) true)
(check-expect (checkIfStudentHaveAllergies student2) false)

;(define (checkIfStudentHaveAllergies student) false)    ; stub
;<template from Student definition>

(define (checkIfStudentHaveAllergies student)
  (student-hasAllergies student))
                

