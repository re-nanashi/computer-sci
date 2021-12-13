;; Employees is Natural(10, 50]
;; interp. number of ski lodge employees
(define employee1 11) 
(define employee2 40) 
(define employee3 50) 

#;
(define (employeeFunc emp)
  (...emp))

;; Template rules used:
;; - atomic non-distinct: Natural(10,50]

;; Employees -> Number
;; produces the total payroll for the quarter given the number of employees
(check-expect (totalPayroll employee1) (* 11 1500))
(check-expect (totalPayroll employee2) (* 40 1500))
(check-expect (totalPayroll employee2) (* 50 1500))

;;(define (totalPayroll employee) 0)     ;stub
;;(define (totalPayroll employee)        ;template
;;  (... employee))

(define (totalPayroll employee)
  (* employee 1500))
