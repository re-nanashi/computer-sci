;; Data definitions:

(define-struct path (room nexts))
;; Path is (make-path String (listof Path))
;; interp. An arbitrary-arity tree of paths.
;; - (make-path room nexts) represents all the paths downward from room
(define P0 (make-path "A" empty)) ; a room from which there are no paths

(define PH 
  (make-path "Porch"
   (list 
    (make-path "Living Room"
      (list (make-path "Dining Room"
              (list (make-path"Kitchen"
                      (list (make-path "Hall"
                              (list (make-path "Study" (list))
                                    (make-path "Bedroom" (list))
                                    (make-path "Bathroom" (list))))))))
            (make-path "Hall"
              (list (make-path "Kitchen"
                      (list (make-path "Dining Room" (list))))
                    (make-path "Study" (list))
                    (make-path "Bedroom" (list))
                    (make-path "Bathroom" (list)))))))))
   
#;
(define (fn-for-path p)
  (local [(define (fn-for-path p)
            (... (path-room p)
                 (fn-for-lop (path-nexts p))))
          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                    (... (fn-for-path (first lop))
                         (fn-for-lop (rest lop)))]))]
    (fn-for-path p)))

;; Result is one of:
;; - Boolean
;; - "never"
;; interp. three possible answers to a question
(define R0 true)
(define R1 false)
(define R2 "never")

#; 
(define (fn-for-result r)
  (cond [(boolean? r) (... r)]
        [else (...)]))

;; Result Result -> Result 
;; produce the logical combination of two results
(check-expect (and-result false false) false)
(check-expect (and-result false true) false)
(check-expect (and-result false "never") false)
(check-expect (and-result true false) false)
(check-expect (and-result true true) true)
(check-expect (and-result true "never") true)
(check-expect (and-result "never" true) true)
(check-expect (and-result "never" false) false)
(check-expect (and-result "never" "never") "never")

(define (and-result r0 r1)
  (cond [(and (boolean? r0) (boolean? r1)) (and r0 r1)]
        [(string? r0) r1]
        [else r0]))

;; Path Path -> Result
;; produce true if to get to room c, needs to pass room b                                   
;; produce false if even without passing to room b, can get to room c                      
;; produce nevercan't get to room c                                                       
(check-expect (always-before--tail PH    "Hall"  "Study")       true)
(check-expect (always-before--tail PH    "Hall"  "Dining Room") false)
(check-expect (always-before--tail PH    "Study" "Hall")        false)
(check-expect (always-before--tail PH    "Dining Room" "Bathroom") false)
(check-expect (always-before--tail PH    "Kitchen" "Porch")     false)
(check-expect (always-before--tail PH    "Porch"   "Kitchen")   true)
(check-expect (always-before--tail PH    "Living Room" "Dining Room") true)
(check-expect (always-before--tail PH    "Study" "Office") "never")
(check-expect (always-before--tail PH    "Dining Room" "Kitchen") false)

(define (always-before--tail p0 b c)
  (local [(define (fn-for-path p passed? todo result)
            (if (string=? (path-room p) c)
              (fn-for-lop todo (and-result result passed))
              (if (string=? (path-room p) b)
                (fn-for-lop (append todo (map (lambda (r) (list r true)) (path-nexts p))) 
                            result)
                (fn-for-lop (append todo (map (lambda (r) (list r passed?)) (path-nexts p)))
                            result))))

          (define (fn-for-lop lop result)
            (cond [(empty? lop) result]
                  [else
                    (local [(define path (first (first todo)))
                            (define passed (second (first todo)))]
                      (fn-for-path path passed? (rest todo) result))]))]
    (fn-for-path p0 false empty "never")))
