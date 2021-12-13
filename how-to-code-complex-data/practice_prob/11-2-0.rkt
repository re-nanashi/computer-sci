;; hellos : Natural -> ListOfString
(check-expect (hellos 2) (cons "hello" (cons "hello" empty)))
(check-expect (hellos 3) (cons "hello" (cons "hello" (cons "hello" empty))))

(define (hellos n)
  (cond [(= n 0) empty]
        [else
          (cons "hello"
                (hellos (sub1 n)))]))

