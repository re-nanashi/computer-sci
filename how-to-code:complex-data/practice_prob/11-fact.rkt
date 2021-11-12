;; Number -> Number
;; produce the n factorial
(check-expect (factorial 3) 6)
(check-expect (factorial 0) 1)
(check-expect (factorial 1) 1)
(check-expect (factorial 5) 120)

(define (factorial n)
  (cond [(<= n 1) 1]
        [else
          (* n (factorial (sub1 n)))]))

;; N N -> N
;; produce the sum of two natural numbers without using +

(check-expect (sum 1 1) 2)
(check-expect (sum 5 0) 5)
(check-expect (sum 50 100) 150)

(define (sum x y)
  (cond [(zero? y) x]
        [else
          (add1 (sum x (sub1 y)))]))

;; Number -> Number 
;; multiply the given number by pi 3.14 without using *
(check-expect (multiply 0) 0)
(check-expect (multiply 2) 6.28)

(define (multiply n)
  (cond [(zero? n) 0]
        [else
          (+ 3.14
             (multiply (sub1 n)))]))


