#lang racket

(provide (all-defined-out))

(define-struct const (int) #:transparent)
(define-struct negate (e) #:transparent)
(define-struct add (e1 e2) #:transparent)
(define-struct multiply (e1 e2) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (const (- (const-int (eval-exp (negate-e e)))))]
        [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                        [v2 (const-int (eval-exp (add-e2 e)))])
                    (const (+ v1 v2)))]
        [(multiply e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                            [v2 (const-int (eval-exp (multiply-e2 e)))]) 
                        (const (* v1 v2)))]
        [else (error "eval-exp expected an exp")]))

(define a-test (eval-exp (multiply (negate (add (const 2) (const 2)))
                                   (const 7))))

