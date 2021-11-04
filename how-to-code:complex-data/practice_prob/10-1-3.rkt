;; ListOfNumbers -> ListOfNumbers
;; converts a list of Fahrenheit measurements to a list of Celsius measurements
(check-expect (convert empty) empty)
(check-expect (convert (cons 32 (cons 212 (cons 86 empty)))) 
              (cons 0 (cons 100 (cons 30 empty))))

(define (convert lof)
  (cond [(empty? lof) empty]
        [else
          (cons (convert_f_c (first lof))
                (convert (rest lof))) ]))

(define (convert_f_c f)
  (/ (* (- f 32) 5)
     9))
