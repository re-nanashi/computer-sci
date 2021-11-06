(define-struct pr (name number))
;; PhoneRecord is (make-pr String Number)
;; interp. a record with a person's name and his/her number.

#; 
(define (fn-for-pr pr)
  (... (pr-name pr)
       (pr-number pr)))

;; Directory is one of:
;; - empty
;; - (cons PhoneRecord Directory)
;; interp. a list of PhoneRecords

#; 
(define (fn-for-dir dir)
  (cond [(empty? dir) (...)]
        [else
          (... (fn-for-pr (first dir))
               (fn-for-dir (rest dir)))]))

;; whose_number : Number Directory -> (pr-name)
;; produces a name(String) of the owner of the given number, otherwise produce "Not found."
(check-expect (whose_number 2029398100 empty) "Not found.")
(check-expect (whose_number 2125551212 
                            (cons (make-pr "ny-information" 2125551212)
                                  (cons (make-pr "dc-information" 2025551212) empty))) 
              "ny-information")

(check-expect (whose_number 2025551212 
                            (cons (make-pr "ny-information" 2125551212)
                                  (cons (make-pr "dc-information" 2025551212) empty))) 
              "dc-information")

;(define (whose_number n dir) "")    ; stub

(define (whose_number n dir)
  (cond [(empty? dir) "Not found."]
        [else
          (if (= n (pr-number (first dir))) 
            (pr-name (first dir)) 
            (whose_number n (rest dir)))]))


;; phone_number : String Directory -> (pr-number)
;; produces the number of the given owner's name, otherwise produce "Not found."
(check-expect (phone_number "san-francisco-info" empty) "Not found.")
(check-expect (phone_number "ny-information" (cons (make-pr "ny-information" 2125551212) empty)) 2125551212)

;(define (phone_number str dir) 0)    ; stub

(define (phone_number str dir)
  (cond [(empty? dir) "Not found."]
        [else
          (if (string=? str (pr-name (first dir))) 
            (pr-number (first dir)) 
            (phone_number str (rest dir)))]))


