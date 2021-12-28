#lang racket

(provide (all-defined-out))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [else 
          (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(define (slow-add x y)
  (letrec ([slow-id (lambda (y z) 
                      (if (= 0 z) 
                           y 
                          (slow-id y (- z 1))))])
           (+ (slow-id x 50000000) y)))

(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

(my-mult 0 (let ([p (my-delay (lambda () (slow-add 3 4)))])
             (lambda () (my-force p))))
