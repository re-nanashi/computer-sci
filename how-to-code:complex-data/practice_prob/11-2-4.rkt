(define (depth dl)
  (cond [(empty? dl) 0]
        [else
          (+ 1 
             (depth (rest dl)))]))

(define (make_deep s n)
  (cond [(zero? n) empty]
        [else
          (cons s
                (make_deep (sub1 n)))]))
