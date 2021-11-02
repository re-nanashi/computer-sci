;; Data definitions:

(define-struct wiz (name wand patronus kids))
;; Wizard is (make-wiz String String String ListOfWizard)
;; interp. a wizard in a descendant family tree
;;          name is the first name
;;          wand is the wood their primary wand is made of ("" if unknown)
;;          patronus is a string ("" if unknown)
;;          kids is their immediate children

;; ListOfWizard is one of:
;; - empty
;; - (cons Wizard ListOfWizard)
;; interp. a list of wizards

(define ARTHUR 
  (make-wiz "Arthur" "" "Weasel"
            (list (make-wiz "Bill" "" "" (list (make-wiz "Victoire" "" "" empty)
                                               (make-wiz "Dominique" "" "" empty)
                                               (make-wiz "Louis" "" "" empty)))
                  (make-wiz "Charlie" "ash" "" empty)
                  (make-wiz "Percy" "" "" (list (make-wiz "Molly" "" "" empty)
                                                (make-wiz "Lucy" "" "" empty)))
                  (make-wiz "Fred" "" "" empty)
                  (make-wiz "George" "" "" (list (make-wiz "Fred" "" "" empty)
                                                 (make-wiz "Roxanne" "" "" empty)))
                  (make-wiz "Ron" "ash" "Jack Russell Terrier" (list (make-wiz "Rose" "" "" empty)
                                                                     (make-wiz "Hugo" "" "" empty)))
                  (make-wiz "Ginny" "" "horse" 
                            (list (make-wiz "James" "" "" empty)
                                  (make-wiz "Albus" "" "" empty)
                                  (make-wiz "Lily" "" "" empty))))))

#;
(define (fn-for-wizard w)
  (... (wiz-name w)
       (wiz-wand w)
       (wiz-patronus w)
       (fn-for-low (wiz-kids w))))

#;
(define (fn-for-low low)
  (cond [(empty? low) (...)]
        [else
          (... (fn-for-wizard (first low))
               (fn-for-low (rest low)))]))

;; ListOfPair is one of:
;; - empty
;; - (cons (list String String) ListOfPair)
;; interp. used to represent an arbitrary number of pairs of strings
(define LOP1 empty)
(define LOP2 (list (list "Harry" "Stag") (list "Hermione" "otter")))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
          (... (first (first lop))
               (second (first lop))
               (fn-for-lop (rest lop)))]))

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (list "a" "b"))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
          (... (first los)
               (fn-for-los (rest los)))]))

;; Functions

;; Wizard -> ListOfPair
;; ListOfWizard -> ListOfPair
;; produce a pair list of every person in the tree
(check-expect (prod_lpair empty) empty)
(check-expect (prod_pair (make-wiz "a" "b" "c" empty)) (list (list "a" "c")))
(check-expect (prod_pair ARTHUR)
              (list 
                (list "Arthur" "Weasel")
                (list "Bill" "")
                (list "Victoire" "")
                (list "Dominique" "")
                (list "Louis" "")
                (list "Charlie" "")
                (list "Percy" "")
                (list "Molly" "")
                (list "Lucy" "")
                (list "Fred" "")
                (list "George" "")
                (list "Fred" "")
                (list "Roxanne" "")
                (list "Ron" "Jack Russell Terrier")
                (list "Rose" "")
                (list "Hugo" "")
                (list "Ginny" "horse")
                (list "James" "")
                (list "Albus" "")
                (list "Lily" "")))

;(define (prod_pair w) lop)        ; stub
;(define (prod_lpair low) lop)        ; stub

(define (prod_pair w)
  (cons (list (wiz-name w) 
              (wiz-patronus w))
        (prod_lpair (wiz-kids w))))

(define (prod_lpair low)
  (cond [(empty? low) empty]
        [else
          (append (prod_pair (first low))
                  (prod_lpair (rest low)))]))

;; Wizard String -> ListOfString
;; ListOfWizard String -> ListOfString
;; produce names of all descendants whose wand is made of given wood
(check-expect (has_lwand empty "x") empty)
(check-expect (has_wand (make-wiz "a" "b" "c" empty) "x") empty)
(check-expect (has_wand (make-wiz "a" "b" "c" empty) "b") (list "a"))
(check-expect (has_wand ARTHUR "ash") (list "Charlie" "Ron"))

(define (has_wand w s)
  (if  (string=? (wiz-wand w) s)
    (cons (wiz-name w) 
          (has_lwand (wiz-kids w) s))
    (has_lwand (wiz-kids w) s)))

(define (has_lwand low s)
  (cond [(empty? low) empty]
        [else
          (append (has_wand (first low) s)
                  (has_lwand (rest low) s))]))

