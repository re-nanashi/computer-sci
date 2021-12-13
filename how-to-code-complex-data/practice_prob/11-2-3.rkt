;; ListOfPosns is one of:
;; - empty
;; - (cons Posn ListOfPosns)

;; f : number -> number
(define (f x)
  (+ (* 3 (* x x))
     (+ (* -6 x)
        -1)))

(define (tabulate_f n)
  (cond [(zero? n) empty]
        [else
          (cons (make-posn n (f n))
                (tabulate_f (sub1 n)))]))

(check-expect (tabulate_f 0) empty)
(check-expect (tabulate_f 4) (cons (make-posn 4 23) 
                                   (cons (make-posn 3 8)
                                         (cons (make-posn 2 -1)
                                               (cons (make-posn 1 -4) 
                                                     empty)))))
