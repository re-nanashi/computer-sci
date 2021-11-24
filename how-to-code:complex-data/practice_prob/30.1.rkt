;;(listof Number) -> (listof Number)
;; given a list of relative numbers, produce a list of absolute values
(check-expect (convert (list 0 50 40 70 30 30)) (list 0 50 90 160 190 220))

(define (convert lon0)
  (local [(define (convert lon prev converted_list)
            (cond [(empty? lon) converted_list]
                  [else
                    (convert (rest lon)
                             (+ prev (first lon))
                             (append converted_list (list (+ (first lon) prev))))]))]
    (convert lon0 0 empty)))

;; (listof String) -> (listof String)
;; remove duplicates from given string
(check-expect (remove_dup (list "a" "b" "b" "b" "b" "b" "b" "b")) (list "a" "b"))
(check-expect (remove_dup (list "a" "b" "b" "b" "c" "c")) (list "a" "b" "c"))


(define (remove_dup los0)
  (local [(define (remove_dup los new_list)
            (cond [(empty? los) new_list]
                  [else
                    (if (false? (member (first los) new_list))
                      (remove_dup (rest los) (append new_list (list (first los))))
                      (remove_dup (rest los) new_list))]))]
    (remove_dup los0 empty)))
