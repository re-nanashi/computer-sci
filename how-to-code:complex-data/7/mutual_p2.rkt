;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mutual_p2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions

(define-struct person (name age kids))
;; Person is (make-person String Natural ListOfPerson)
;; interp. A person with name, age, and their children

(define P1 (make-person "N1" 5 empty))
(define P2 (make-person "N2" 25 (list P1)))
(define P3 (make-person "N3" 15 empty))
(define P4 (make-person "N4" 45 (list P3 P2)))

#;
(define (fn-for-person p)
  (... (person-name p)                      ; String
       (person-age p)                       ; Natural
       (fn-for-lop (person-kids p))))       ; ListOfPerson -> ???

;; ListOfPerson is one of:
;; - empty
;; - (cons Person ListOfPerson)
;; interp. a list of persons
#;
(define (fn-for-lop lop)
  (cond [(empty lop) (...)]
        [else
          (... (fn-for-person (first person))
               (fn-for-lop (rest person)))]))

;; Person String -> Integer or false
;; ListOfPerson String -> Interger or false
;; search the given tree for an element with the given name; produce the person's age if found; false otherwise
(check-expect (lookup_lop "N1" empty) false)
(check-expect (lookup_person "N1" P3) false)
(check-expect (lookup_person "N1" P1) 5)
(check-expect (lookup_person "N1" P3) false)
(check-expect (lookup_person "N1" P4) 5)
(check-expect (lookup_person "N1" P2) 5)

;(define (lookup_person p s) false)     ; stub
;(define (lookup_lop lop s) false)     ; stub

(define (lookup_person s p)
  (if (string=? s (person-name p))                       ; String
       (person-age p)                       ; Natural
       (lookup_lop s (person-kids p))))       ; ListOfPerson -> ???)

(define (lookup_lop s lop)
  (cond [(empty? lop) false]
        [else
          (if (not (false? (lookup_person s (first lop)))) 
               (lookup_person s (first lop))
               (lookup_lop s (rest lop)))]))
