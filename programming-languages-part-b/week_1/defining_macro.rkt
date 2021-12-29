#lang racket

(provide (all-defined-out))

;; a cosmetic macro -- adds then, else
; (my-if foo then bar else baz) -> (if foo bar baz)
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))


;; a macro to replace a nexpression with another one
; (comment-out (car null) (+ 3 4)) -> (+ 3 4)
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))

; makes it so users do *not* wirte the thunk when using my-delay
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))

; this is really bad because it evaluates e multiple times
(define-syntax my-force-macro1
  (syntax-rules ()
    [(my-force e)
     (if (mcar e)
         (mcdr e)
         (begin (set-mcar! e #t)
                (set-mcdr! e ((mcdr e)))
                (mcdr e)))]))

; do *not* do this either because a function is exactly what 
(define-syntax my-force-macro2
  (syntax-rules ()
    [(my-force e)
     (let ([x e])
       (if (mcar x)
           (mcdr x)
           (begin (set-mcar! x #t)
                  (set-mcdr! x ((mcdr x)))
                  (mcdr x))))]))

; just do this:
(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th))))
