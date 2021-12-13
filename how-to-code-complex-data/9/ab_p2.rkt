;; =================
;; Data definitions:

(define-struct photo (location album favourite))
;; Photo is (make-photo String String Boolean)
;; interp. a photo having a location, belonging to an album and having a
;; favourite status (true if photo is a favourite, false otherwise)
(define PHT1 (make-photo "photos/2012/june" "Victoria" true))
(define PHT2 (make-photo "photos/2013/birthday" "Birthday" true))
(define PHT3 (make-photo "photos/2013/birthday" "Birthday" true))
(define PHT4 (make-photo "photos/2012/august" "Seattle" false))

#;
(define (fn-for-p p)
  (... (photo-location p)
       (photo-album p)
       (photo-favourite p)))

;; ListOfPhotos is one of:
;; - empty
;; - (cons Photo ListOfPhotos)
;; interp. a list of Photos
(define L0 empty)
(define L1 (list PHT1))
(define L2 (list PHT1 PHT2))
(define L3 (list PHT4))
(define L4 (list PHT1 PHT2 PHT3 PHT4))

#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
          (... (fn-for-p (first lop))
               (fn-for-lop (rest lop)))]))


;; =================
;; Functions:

;; String ListOfPhotos -> ListOfPhotos
;; produce a list of only those photos that are favourites that belong to an album
(check-expect (favourites "Tour" L0) empty)
(check-expect (favourites "Tour" L1) empty)
(check-expect (favourites "Victoria" L1) L1)
(check-expect (favourites "Birthday" L4) (list PHT2 PHT3))
(check-expect (favourites "Seattle" L3) empty)

(define (favourites album_name lop)
  (local [(define (favourite? p)
            (and (string=? (photo-album p) album_name)
                  (photo-favourite p)))]
    (filter favourite? lop)))
