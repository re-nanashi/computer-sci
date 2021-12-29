#lang racket

(provide (all-defined-out))

;; a loop that executes body hi - lo times
;; notice use of local variables
(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi do body)
     (let ([l lo]
           [h hi])
       (letrec ([loop (lambda (it)
                        (if (> it h)
                             #t
                             (begin body (loop (+ it 1)))))])
         (loop l)))]))

;; the special ... lets us take any number of arg
;; NOTE: nothing prevents infinite code generation except 
;; the macro definer being careful
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body) body]
    [(my-let* ([var0 val0] 
               [var-rest val-rest] ...) body)
     (let ([var0 val0])
       (my-let* ([var-rest val-rest] ...)
                body))]))
