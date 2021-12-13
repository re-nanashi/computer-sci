(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;; - false
;; - (make-node Integer BST BST)
;; interp. false means no BST, or empty BST
;;          key is the node key
;;          val is the node val
;;          l and r are left and right substrees
;; INVARIANT: for a given node:
;;          key is > all keys in its left child
;;          key is < all keys in its right child
;;          the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define )
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             false))
(define BST10 (make-node 10 "why" BST3 BST42))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
          (... (node-key t)
               (node-key t)
               (fn-for-bst (node-l t))
               (fn-for-bst (node-r t)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic-distinct: false
;; - compound: (make-node Integer String BST BST)
;; - self-reference: (node-l t) has type BST
;; - self-reference: (node-r t) has type BST


;; Integer String BST -> BST
;; insert a node appropriately into a tree
(check-expect (insert 1 "lov" BST0) (make-node 1 "lov" false false))
(check-expect (insert 8 "hey" BST7) (make-node 7 "ruf" false (make-node 8 "hey" false false)))


;(define (insert key str t) t)  ; stub

(define (insert key val bst)
  (cond [(false? bst) (make-node key val false false)]
        [else
          (if (< key (node-key bst)) 
              (make-node (node-key bst)
                         (node-val bst) 
                         (insert key val (node-l bst))
                         (node-r bst))
              (make-node (node-key bst)
                         (node-val bst) 
                         (node-l bst)
                         (insert key val (node-r bst))))]))


