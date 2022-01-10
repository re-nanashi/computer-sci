#lang racket
(provide (all-defined-out))

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

; BT -> Integer

(define (tree-height bt)
  (letrec ([f (lambda (t ht)
                (if (btree-leaf? t)
                  ht
                  (max (f (btree-node-left t), (+ ht 1)) 
                       (f (btree-node-right t), (+ ht 1)))))])
    (f bt 0)))

(define (sum-tree bt)
  (if (btree-leaf? bt)
    0
    (+ (btree-node-value bt) 
       (btree-node-left bt) 
       (btree-node-right bt))))
