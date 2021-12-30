#lang racket

(provide (all-defined-out)) 

; Integer Integer Integer -> (listof Integer)
; produce a list of numbers from lo to hi with interval st
(define (sequence lo hi st)
  (cond [(> lo hi) empty]
        [else 
          (cons lo 
                (sequence (+ lo st) hi st))]))

; (listof String) String -> (listof String)
; returns a list of string with suffix attached to all elements of the list
(define (string-append-map los s)
  (map (lambda (str) (string-append str s)) los))

; (listof x) Integer -> x
; produce the ith element of the list by dividing n to length of list
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (cond [(= n 0) empty]
        [else 
          (cons (car (s))
                (stream-for-n-steps (cdr (s)) (sub1 n)))]))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (modulo x 5) 0) (- 0 x) x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) 
                (cons x (lambda () 
                          (f (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car (x)))
                      (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

; !!!
(define (cycle-lists xs ys)
  (cond []))
