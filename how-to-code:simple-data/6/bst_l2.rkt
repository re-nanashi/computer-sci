(define-struct node (key val l r))
;; BST (Binary Search Tree) is one of:
;; - false
;; - (make-node Integer String BST BST)
;; interp. false means no BST, or empty
;;          key is the node key
;;          val is the node val
;;          l and r are left and right subtrees
;; INVARIANTS: for a given node:
;;          key is > all key in the l (eft) child 
;;          key is < all key in the r (ight) child 
;;          the same key never appears twice in the tree
(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily" 
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
          (... (node-key t)
               (node-val t)
               (fn-for-bst (node-l t))
               (fn-for-bst (node-r t)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: false
;; - compound: (make-node Integer String BST BST)
;; - self-reference: (node-l) is BST
;; - self-reference: (node-r) is BST

;; BST Natural -> String or false
;; try to find node with given key, if found produce value; if not found produce false
(check-expect (lookup BST10 50) "dug")
(check-expect (lookup BST10 4) "dcj")
(check-expect (lookup BST10 2) false)
(check-expect (lookup BST10 5) false)
(check-expect (lookup BST10 2) false)
(check-expect (lookup BST1   1) "abc")
(check-expect (lookup BST1   0) false) ;L fail
(check-expect (lookup BST1  99) false) ;R fail
(check-expect (lookup BST10  1) "abc") ;L L succeed
(check-expect (lookup BST10  4) "dcj") ;L R succeed
(check-expect (lookup BST10 27) "wit") ;R L succeed
(check-expect (lookup BST10 50) "dug") ;R R succeed
;(define (lookup lst k) "") ; stub

(define (lookup lst n)
  (cond [(false? lst) false]
        [else
          (cond [(equal? (node-key lst) n) 
                 (node-val lst)]  
                [(> (node-key lst) n) 
                 (lookup (node-l lst) n)]
                [else
                  (lookup (node-r lst) n)])]))
