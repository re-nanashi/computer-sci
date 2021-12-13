;; Natural -> (listof Natural) 
;; produce (cons n (cons n-1 ... empty)), not including 0
(check-expect (to-list 0) empty)
(check-expect (to-list 1) (list 1))
(check-expect (to-list 3) (list 1 2 3))

;(define (to-list n) empty) ;stub

#;
(define (to-list n)
  (cond [(zero? n) empty]
        [else
         (append (to-list (sub1 n))
                 (list n))]))

(define (to-list n0)
  (local [(define (to-list n acc)
            (cond [(zero? n) acc]
                  [else
                    (to-list (sub1 n)
                             (cons n acc))]))]
    (to-list n0 empty)))
