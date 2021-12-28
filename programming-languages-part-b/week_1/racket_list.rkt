; Section 5: Racket Lists
#lang racket

(provide (all-defined-out))

; (listof Integer) -> Integer
; produce the sum of all the numbers in a list
(define (sum loi)
  (cond [(empty? loi) 0]
        [else
          (+ (first loi)
             (sum (rest loi)))]))

; List List -> List
; append two given list
(define (my_append xs ys)
  (cond [(empty? xs) ys]
        [else
          (cons (first xs)
                (my_append (rest xs) ys))]))

; Fn List -> List
; map
(define (my_map f xs)
  (cond [(empty? xs) empty]
        [else
          (cons (f (first xs))
                (my_map f (rest xs)))]))
