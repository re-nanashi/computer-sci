;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname mutual_p1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; =============
;; Constants:

(define WIDTH 700)
(define HEIGHT 400)
(define X_SPACE 50)
(define Y_SPACE 50)

(define MTS (rectangle WIDTH HEIGHT "solid" "white"))

;; =============
;; Data definitions:

(define-struct dir (name sub_dirs images))
;; Directory is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;          of sub_dirs and a list of images.

#;
(define (fn-for-dir d)
  (... (dir-name d)                          ; String
       (fn-for-lod (dir-sub_dirs d))         ; ListOfDir
       (fn-for-loi (dir-images d))))         ; ListOfImage

;; ListOfDir is one of:
;; - empty
;; - (cons Dir ListOfDir)
;; interp. A list of diretories, this represents the sub-directories of a directory

#; 
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
          (... (fn-for-dir (first lod))
               (fn-for-lod (rest lod)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - reference: (first lod) is Directory
;; - self-reference: (rest lod) is ListOfDir

;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. A list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
          (... (first loi)
               (fn-for-loi (rest loi)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - reference: (first lod) is Image
;; - self-reference: (rest lod) is ListOfImage

(define I1 (square 10 "solid" "red"))
(define I2 (square 10 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; Directory -> Natural
;; ListOfDir -> Natural or false
;; ListOfImage -> Natural
;; calculate the total size (* width height) of all the images in a directory and its sub.
(check-expect (get_total_size D4) (+ 100 100))
(check-expect (get_total_size D5) 182)
(check-expect (get_total_size D6) (+ 100 100 182))
(check-expect (get_total_dir_size (list D4 D5)) (+ 100 100 182))

;(define (get_total_size dir) 0)         ; stub

(define (get_total_size d)
  (if (empty? (dir-sub_dirs d)) 
    (get_total_size_list (dir-images d)) 
    (get_total_dir_size (dir-sub_dirs d))))         

;; ListOfDir -> Integer or false
;; produce the total size of the images inside a directory.

(define (get_total_dir_size lod)
  (cond [(empty? lod) 0]
        [else
          (if (not (empty? (get_total_size (first lod))))
            (+ (get_total_size (first lod)) (get_total_dir_size (rest lod)))
            (get_total_dir_size (rest lod)))]))

;; ListOfImage -> Natural
;; given a list of images calculate the total size of all the images
(check-expect (get_total_size_list (list I1 I2)) (+ 100 100))
(check-expect (get_total_size_list (list I3)) (+ 0 182))

;(define (get_total_size_list loi) 0)       ;stub

(define (get_total_size_list loi)
  (cond [(empty? loi) 0]
        [else
          (+   (get_size (first loi))
               (get_total_size_list (rest loi)))]))

;; Image -> Natural
;; produce the size (* width height) of an image.
(check-expect (get_size I1) 100)
(check-expect (get_size I2) 100)
(check-expect (get_size I3) 182)

;(define (get_size img) 0)           ; stub

(define (get_size img)
  (* (image-width img) (image-height img)))

;; ===================
;; Render Directory

;; Directory -> Image
;; ListOfDir -> Image
;; ListOfImage -> Image
;; render the images on the directory
(check-expect ())

(define (render_dir_img d) (empty-scene))       ; stub


