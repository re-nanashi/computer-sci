;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mutual_l1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Data definitions:

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;          If data is 0, then subs is considered to be list of sub elements.
;;          If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;; - empty
;; - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))

#;
(define (fn-for-element e)
  (... (elt-name e)                  ;String
       (elt-data e)                  ;Integer
       (fn-for-loe (elt-subs e))))   ;ListOfElement

#;
(define (fn-for-loe loe)
  (cond [(empty? loe) (...)]                        ;Base
        [else
          (... (fn-for-element (first loe))
               (fn-for-loe (rest loe)))]))

;; Element -> Natural
;; produces the sum of all data in the tree
(check-expect (sum_of_data F1) 1)   
(check-expect (sum_of_data D5) 3)   
(check-expect (sum_of_data D4) (+ 1 2))   

;(define (sum_of_data e) 0)      ; stub
; <template from Element>

(define (sum_of_data e)
  (if (zero? (elt-data e))                     
    (iter (elt-subs e))
    (elt-data e)))

(define (iter loe)
  (cond [(empty? loe) 0]
        [else
          (+ (sum_of_data (first loe))
             (iter (rest loe)))]))

;; Element -> ListOfString
;; ListOfElement -> ListOfString
;; produce all the names of all elements in the tree
(check-expect (create_list_of_names F1) (list "F1"))      ; base
(check-expect (create_list_of_lnames empty) empty)              ; base
(check-expect (create_list_of_names F2) (list "F2"))      
(check-expect (create_list_of_names D5) (list "D5" "F3"))     
(check-expect (create_list_of_names D6) (list "D6" "D4" "F1" "F2" "D5" "F3"))     

;(define (create_list_of_names e) empty)     ; stub
;(define (create_list_of_lnames loe) empty)  ; stub


(define (create_list_of_names e)
  (cons (elt-name e)
        (create_list_of_lnames (elt-subs e))))             ; if 0, surely will produce empty
#; 
(define (create_list_of_names e)            
  (if (zero? (elt-data e)) 
    (create_list_of_lnames (elt-subs e))
    (cons (elt-name e) empty)))

(define (create_list_of_lnames loe)
  (cond [(empty? loe) empty]                        
        [else
          (append (create_list_of_names (first loe))
                  (create_list_of_lnames (rest loe)))]))


(define-struct person (name age children))
;; Person is (make-person String Natural ListOfPerson)
;; interp. a person with first name, age and a list of their children.

(define P1 (make-person "N1" 5 empty))
(define P2 (make-person "N2" 25 (list P1)))
(define P3 (make-person "N3" 15 empty))
(define P4 (make-person "N4" 45 (list P2 P3)))
(define P5 (make-person "N5" 19 (list P4 P3)))

#;
(define (fn-for-person p)
  (... (person-name p)                      ; String
       (person-age p)                       ; Natural
       (fn-for-lop (person-children p))))

;; ListOfPerson is one of:
;; - empty
;; (cons Person ListOfPerson)
;; interp. a list of persons
#;
(define (fn-for-lop lop) 
  (cond [(empty? lop) (...)]
        [else
          (... (fn-for-person (first lop))
               (fn-for-lop (rest lop)))]))

;; Person -> ListOfString
;; ListOfPerson -> ListOfString
;; produce a list of the names of the persons under 20

(check-expect (names_under_20 P1) (list "N1"))
(check-expect (names_under_l20 empty) empty)
(check-expect (names_under_20 P2) (list "N1"))
(check-expect (names_under_20 P4) (list "N1" "N3"))
(check-expect (names_under_20 P5) (list "N5" "N1" "N3" "N3"))


(define (names_under_20 p)
  (if (< (person-age p) 20) 
    (cons (person-name p)
          (names_under_l20 (person-children p)))
    (names_under_l20 (person-children p))))

(define (names_under_l20 lop) 
  (cond [(empty? lop) empty]
        [else
          (append (names_under_20 (first lop))
                  (names_under_l20 (rest lop)))]))

;; String Element -> Integer or false
;; String ListOfElement -> Integer or false???
;; search the given tree for an element with the given name, produce data if found; false otherwise
(check-expect (find_loe "F3" empty) false)
(check-expect (find_loe "F3" F1) false)
(check-expect (find_loe "F3" F3) 3)
(check-expect (find_loe "F3" D4) false)
(check-expect (find_loe "F1" D4) 1)
(check-expect (find_loe "F2" D4) 2)
(check-expect (find_loe "D4" D4) 0)
(check-expect (find_loe "F3" D6) 3)

;(define (find_element n e) false)   ;stub
;(define (find_loe n loe) false)   ;stub 

(define (find_element n e)
  (if (string=? n (elt-name e))                   
      (elt-data e)                  
      (find_loe n (elt-subs e))))  

(define (find_loe n loe)
  (cond [(empty? loe) false]                        ;Base
        [else
          (if (not (false? (find_element n (first loe))))
            (find_element n (first loe))
            (find_loe n (rest loe)))]))

