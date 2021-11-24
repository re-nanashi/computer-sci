;; Problem 1

;; (listof Strings) -> Natural
;; produce the length of the longest string inside the list
(check-expect (longest_length (list "a" "bb" "cccc")) 4)
(check-expect (longest_length (list "a" "bb")) 2)
(check-expect (longest_length (list "a")) 1)
(check-expect (longest_length empty) 0)

(define (longest_length los0)
  (local [(define (fn-for-los los acc)
            (cond [(empty? los) acc]
                  [else
                    (fn-for-los (rest los)
                                (if (> (string-length (first los)) acc) 
                                  (string-length (first los))
                                  acc))]))]
    (fn-for-los los0 0)))

;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)

#;
(define (fact n)
  (cond [(zero? n) 1]
        [else 
         (* n (fact (sub1 n)))]))

(define (fact n0)
  (local [(define (fact n acc)
            (cond [(zero? n) acc]
                  [else
                    (fact (sub1 n)
                          (* acc n))]))]
    (fact n0 1)))

;; Problem 3

(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))
          
          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))
          
          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else 
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))

;; Region -> Natural
;; produce the number of regions within the given region
(check-expect (fn-for-region CANADA) 7)
(check-expect (fn-for-region VANCOUVER) 1)
(check-expect (fn-for-region BC) 3)

(define (fn-for-region r0)
  (local [(define (fn-for-region r todo acc)
            (fn-for-lor (append (region-subregions r) todo) (add1 acc)))

          (define (fn-for-lor lor acc)
            (cond [(empty? lor) acc]
                  [else
                    (fn-for-region (first lor) 
                                   (rest lor)
                                   acc)]))]
    (fn-for-region r0 empty 0)))
