;; =================
;; Data definitions:

(define-struct movie (title budget year))
;; Movie is (make-movie String Number Number)
;; interp. (make-movie) is a movie with
;;          title is the movie's title
;;          budget is the movie's budget
;;          year is when the movie was released
(define movie1 (make-movie "Titanic" 200000000 1997))
(define movie2 (make-movie "Avatar" 237000000 2009))
(define movie3 (make-movie "The Avengers" 220000000 2012))
(define movie4 (make-movie "Test" 2000 2012))

(define (movieFunc m)
  (... (movie-title m)          ; String
       (movie-budget m)         ; Number
       (movie-year m)))         ; Number

;; Template rules used:
;; - Compound: 3 fields

;; =================
;; Functions:

;; movie movie -> String
;; produces the title of the more recent of two movies
(check-expect (getMoreRecentMovie movie1 movie2) (movie-title movie2))
(check-expect (getMoreRecentMovie movie2 movie1) (movie-title movie2))
(check-expect (getMoreRecentMovie movie3 movie4) 
              (string-append (movie-title movie3) " and " (movie-title movie4) " were both released in " (number->string (movie-year movie3)) "."))

;(define (getMoreRecentMovie movie1 movie2) "")             ;stub
;<template from Movie definition>

(define (getMoreRecentMovie movie1 movie2)
  (cond [(equal? (movie-year movie1) (movie-year movie2)) 
         (string-append (movie-title movie1) " and " (movie-title movie2) " were both released in " (number->string (movie-year movie1)) ".")]
        [(> (movie-year movie1) (movie-year movie2)) (movie-title movie1)]
        [else (movie-title movie2)]))


