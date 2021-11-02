;;Data definitions:

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;; - empty
;; - (cons "A" Pattern)
;; - (cons "N" Pattern)
;; interp.
;;  A pattern describing certain ListOf1String
;; "A" means the corresponding letter must be alphabetic
;; "N" means it must be numeric. For example:
;;      (list "A" "N" "A" "N" "A" "N")
;; describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;; - empty
;; - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4"))

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric
(check-expect (is_alphabetic " ") false)
(check-expect (is_alphabetic "1") false)
(check-expect (is_alphabetic "a") true)
(check-expect (is_numeric " ") false)
(check-expect (is_numeric "1") true)
(check-expect (is_numeric "a") false)

(define (is_alphabetic ls) (char-alphabetic? (string-ref ls 0)))
(define (is_numeric ls) (char-numeric? (string-ref ls 0)))


;; Pattern ListOf1String -> Boolean
;; produce true if the pattern matches the ListOf1String 
(check-expect (does_pattern_match empty empty) true)
(check-expect (does_pattern_match PATTERN1 empty) false)
(check-expect (does_pattern_match empty LOS1) true)
(check-expect (does_pattern_match PATTERN1 LOS1) true)
(check-expect (does_pattern_match PATTERN1 (list "1" "6" "T" "1" "Z" "4")) false)
(check-expect (does_pattern_match PATTERN1 (list "V" "6" "T" "1" "Z" "4" "5")) true)
(check-expect (does_pattern_match PATTERN1 (list "V" "6")) false)


;(define (does_pattern_match p los) false)   ; stub

(define (does_pattern_match p los) 
  (cond [(empty? p) true]
        [(empty? los) false]
        [else
          (and (check (first p) (first los)) 
               (does_pattern_match (rest p) (rest los)))]))

;; 1String String -> Boolean
(define (check p s)
  (cond [(string=? p "A")
         (is_alphabetic s)]
        [(string=? p "N")
          (is_numeric s)]))
