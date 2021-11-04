;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. a directory in the organizer, with a name, a list of of sub-dirs
;;          and a list of images.

;; ListOfDir is one of:
;; - empty
;; - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-dirs of a directory

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. A list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; Directory -> Natural
;; ListOfDir -> Natural
;; ListOfImage -> Natural
;; calculate the total size (* width height of all the images in a dir)

(check-expect (total_size D4) (+ 100 144))
(check-expect (total_size D5) 182)
(check-expect (total_size D6) (+ 100 144 182))

(define (total_size d)
  (local [
          (define (get_total_size d)
            (+ (get_total_dir_size (dir-sub_dirs d))
               (get_total_size_list (dir-images d))))

          (define (get_total_dir_size lod)
            (cond [(empty? lod) 0]
                  [else
                   (+ (get_total_size (first lod))
                      (get_total_dir_size (rest lod)))]))

          (define (get_total_size_list loi)
            (cond [(empty? loi) 0]
                  (else
                   (+ (get_size (first loi))
                      (get_total_size_list (rest loi))))))

          (define (get_size img)
            (* (image-width img)
               (image-height img)))
          ]
    (get_total_size d)))
