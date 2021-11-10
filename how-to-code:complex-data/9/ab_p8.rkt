(require 2htdp/image)

;; =================
;; Data definitions:

(define-struct dir (name sub_dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;          of sub-dirs and a list of images.

;; ListOfDir is one of:
;; - empty
;; - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;          a directory.

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))
(define D7 (make-dir "D7" (list D6 D4 D5) empty))
(define D8 (make-dir "D8" (list D6 D7) empty))
(define D9 (make-dir "D9" (list D8 D7) empty))

#;
(define (fold_dir d)
  (local [(define (fn-for-dir d)
            (... (dir-name d)
                 (fn-for-lod (dir-sub_dirs d))
                 (fn-for-loi (dir-images d))))

          (define (fn-for-lod lod)
            (cond [(empty? lod) (...)]
                  [else
                    (... (fn-for-dir (first lod))
                         (fn-for-lod (rest lod)))]))

          (define (fn-for-loi loi)
            (cond [(empty? loi) (...)]
                  [else
                    (... (first loi)
                         (fn-for-loi (rest loi)))]))]
    (fn-for-dir d)))

;; =================
;; Functions:

;; (String Y Z -> X) (X Y -> Y) (Image Z -> Z) Y Z Dir -> X
;; the abstract fold function for Dir
(check-expect (fold_dir make-dir cons cons empty empty D6) D6)
(check-expect  (local [(define (c1 n rlod rloi) (+ rlod rloi))
                       (define (c2 rdir rlod)   (+ 1 rdir))
                       (define (c3 img rloi)    (+ 1 rloi))]          
                 (fold_dir c1 c2 c3 0 0 D6))
               3)

(define (fold_dir c1 c2 c3 b1 b2 d)
  (local [(define (fn-for-dir d)
            (c1 (dir-name d)                    ; -> X
                (fn-for-lod (dir-sub_dirs d))
                (fn-for-loi (dir-images d))))

          (define (fn-for-lod lod)              ; -> Y
            (cond [(empty? lod) b1]
                  [else
                    (c2 (fn-for-dir (first lod))
                        (fn-for-lod (rest lod)))]))

          (define (fn-for-loi loi)              ; -> Z
            (cond [(empty? loi) b2]
                  [else
                    (c3 (first loi)
                        (fn-for-loi (rest loi)))]))]
    (fn-for-dir d)))

;; Dir -> Number
;; produce the number of images in the directory and its sub-directories
(check-expect (count_images D4) 2)
(check-expect (count_images D5) 1)
(check-expect (count_images D6) (+ 2 1))

(define (count_images d) 
  (local [(define (c1 n lod loi) 
            (+ lod loi))
          (define (c3 img loi)
            (+ 1 loi))]
    (fold_dir c1 + c3 0 0 d)))

;; Dir String -> Boolean
;; produce true if the string is equal to dir-name, otherwise produce false.
(check-expect (search_dir D4 "D4") true)
(check-expect (search_dir D4 "D5") false)
(check-expect (search_dir D6 "D5") true)
(check-expect (search_dir D6 "D4") true)
(check-expect (search_dir D6 "D6") true)
(check-expect (search_dir D6 "D7") false)
(check-expect (search_dir D4 "D8") false)

(define (search_dir d str)
  (local [
          (define (c1 n lod loi)
            (cond [(string=? n str) true]
                  [else lod]))
          (define (c2 d lod) 
            (if (false? d)
              lod
              true))
          (define (c3 img loi)
            false)]
    (fold_dir c1 c2 c3 false 0 d)))

;; Problem D:
;; No, although it works, it needs to search the 
;; whole tree even if the name is already found.

(define (make-skinny n)
  (cond [(zero? n) (make-dir "DY" empty empty)]
        [else
         (make-dir "X" (list (make-skinny (sub1 n))) empty)]))

(time (search_dir (make-skinny 20) "DY"))
(time (search_dir (make-skinny 11) "DY"))
(time (search_dir (make-skinny 12) "DY"))
(time (search_dir (make-skinny 13) "DY"))
(time (search_dir (make-skinny 14) "DY"))
(time (search_dir (make-skinny 15) "DY"))
(time (search_dir (make-skinny 16) "DY"))
