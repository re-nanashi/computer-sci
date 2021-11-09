(require 2htdp/image)

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

; (define (positive-only lon) empty)    ; stub

(define (positive-only lon)
  (filter2 lon positive?))

;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

; (define (negative-only lon) empty)    ; stub

(define (negative-only lon)
  (filter2 lon negative?))

;; (listof X) (X -> Boolean) -> (listof X)
;; given a list and a fn, call fn to all items on the list.
(check-expect (filter2 empty negative?) empty)
(check-expect (filter2 (list 1 -2 3 -4) positive?) (list 1 3))
(check-expect (filter2 (list 1 -2 3 -4) negative?) (list -2 -4))

(define (filter2 lon fn)
  (cond [(empty? lon) empty]
        [else
          (if (fn (first lon))
            (cons (first lon)
                  (filter2 (rest lon) fn))
            (filter2 (rest lon) fn))]))

;; (X Y -> Y) Y (listof X) -> Y
;; the abstract fold function for (listof X)
(check-expect (fold + 0 (list 1 2 3)) 6)
(check-expect (fold * 1 (list 1 2 3)) 6)
(check-expect (fold string-append "" (list "a" "bc" "def")) "abcdef")

(define (fold fn b lox)
  (cond [(empty? lox) b]
        [else
          (fn (first lox)
              (fold fn b (rest lox)))]))

;; (listof Number) -> Number
;; add up all number in list
(check-expect (sum empty) 0)
(check-expect (sum (list 2 3 4)) 9)

;(define (sum lon) 0) ; stub

(define (sum lon) (fold + 0 lon))


;; (listof Image) -> Image
;; juxtapose all images beside each other
(check-expect (juxtapose empty) (square 0 "solid" "white"))
(check-expect (juxtapose (list (triangle 6 "solid" "yellow")
                               (square 10 "solid" "blue")))
              (beside (triangle 6 "solid" "yellow")
                      (square 10 "solid" "blue")
                      (square 0 "solid" "white")))

;(define (juxtapose loi) (square 0 "solid" "white")) ;stub

(define (juxtapose loi) (fold beside (square 0 "solid" "white") loi))

;; (listof X) -> (listof X)
;; produce copy of list
(check-expect (copy-list empty) empty)
(check-expect (copy-list (list 1 2 3)) (list 1 2 3))

;(define (copy-list lox) empty) ;stub

(define (copy-list lox) (fold cons empty lox))

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements
(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))

#;
(define (fn-for-element e)
  (local [(define (fn-for-element e)
            (... (elt-name e)    
                 (elt-data e)    
                 (fn-for-loe (elt-subs e))))

          (define (fn-for-loe loe)
            (cond [(empty? loe) (...)]
                  [else
                   (... (fn-for-element (first loe))
                        (fn-for-loe (rest loe)))]))]
    (fn-for-element e)))

;; (String Integer Y -> X) (X Y -> Y) Y Element -> X
;; the abstract fold function for Element 
(check-expect (local [(define (c1 n d los) (cons n los))]
                (fold_element c1 append empty D6))
              (list "D6" "D4" "F1" "F2" "D5" "F3"))

(define (fold_element c1 c2 b e)
  (local [(define (fn-for-element e) ;-> X
            (c1 (elt-name e)    
                (elt-data e)    
                (fn-for-loe (elt-subs e))))

          (define (fn-for-loe loe)  ;-> Y
            (cond [(empty? loe) b]
                  [else
                   (c2 (fn-for-element (first loe))
                       (fn-for-loe (rest loe)))]))]
    (fn-for-element e)))

;; (X -> Y) ((X -> Y) Y) Y (listof X) -> Y
;; the abstract fold function for Element
(define (fold_el fun1 fun2 b lst)
  (cond [(empty? lst) b]
        [else
          (fun2 (fun1 (first lst))
                (fold_el fun1 fun2 b (rest lst)))]))

;; Element -> Integer
;; produce the sum of all the data in element (and its subs)
(check-expect (sum-data F1) 1)
(check-expect (sum-data D5) 3)
(check-expect (sum-data D4) (+ 1 2))
(check-expect (sum-data D6) (+ 1 2 3))

;(define (sum-data e) 0) ;stub

#;
(define (sum-data e) 
  (local [(define (sum1 e)
            (+ (elt-data e)
               (fold_el sum1 + 0 (elt-subs e))))]
    (sum1 e)))

(define (sum-data e)
  (local [(define (sum1 n d los) (+ d los))]
    (fold_element sum1 + 0 e)))

;; Element -> ListOfString
;; produce list of the names of all the elements in the tree
(check-expect (all-names F1) (list "F1"))
(check-expect (all-names D5) (list "D5" "F3"))
(check-expect (all-names D4) (list "D4" "F1" "F2"))
(check-expect (all-names D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))
               
;(define (all-names e) empty) ;stub

#;
(define (all-names e) 
  (local [(define (prod e)
            (cons (elt-name e)
                  (fold_el prod append empty (elt-subs e))))]
    (prod e)))

(define (all-names e)
  (local [(define (prod n d los) (cons n los))]
    (fold_element prod append empty e)))
