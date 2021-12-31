#lang racket

(provide (all-defined-out)) 

(define (sequence lo hi st)
  (cond [(> lo hi) empty]
        [else 
          (cons lo (sequence (+ lo st) hi st))]))

(define (string-append-map los s)
  (map (lambda (str) (string-append str s)) los))

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

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) 
                                      (list-nth-mod ys x)) 
                                  (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([vl (vector-length vec)]
           [f (lambda (x)
                (if (equal? vl x) 
                  false
                  (let ([e (vector-ref vec x)])
                    (if (and (pair? e) 
                             (equal? (car e) v)) 
                      e 
                      (f (add1 x))))))]) 
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [itr 0]
           [f (lambda (x) 
                (let ([ans (vector-assoc x cache)])
                  (if ans 
                    ans
                    (let ([new-ans (assoc x xs)])
                     (begin 
                       (vector-set! cache itr new-ans) 
                       (set! itr (if (equal? (add1 itr) n) 0 (add1 itr)))
                      new-ans)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([x e1])
       (letrec ([f (lambda () 
                     (let ([y e2]) 
                       (if (or (not (number? y)) (>= y x)) 
                         #t 
                         (f))))])
         (f)))]))
