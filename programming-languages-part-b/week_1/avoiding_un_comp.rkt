; Section 5: Avoid Unnecessary Computations
#lang racket

(provide (all-defined-out))

(define (slow-add x y)
  (letrect ([slow-id (lambda (y z)
                       (if (= 0 z)
                            y
                            (slow-id y (- z 1))))])
           (+ (slow-id x 50000000) y)))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [else
          (+ (y-thunk) (my-mult (- x 1) y-thunk))]))
