(require 2htdp/image)

;; Constants:

(define TEXT_SIZE 14)
(define TEXT_COLOR "black")

(define KEY_VAL_SEPARATOR ":" )

(define MTTREE (rectangle 20 1 "solid" "white"))

;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of;
;; - false
;; (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;          key is the node key
;;          val is the node val
;;          l and r are left and right subtrees
;; INVARIANT: for a given node:
;;      key is > all the keys in the left child
;;      key is < all the keys in the right child
;;      the same key never appears twice in the tree
(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false))
(define BST4 (make-node 4 "dcj" false BST7))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily" 
             (make-node 27 "wit" (make-node 14 "olp" false false) false) 
             (make-node 50 "dug" false false)))
(define BST10 (make-node 10 "why" BST3 BST42))
(define BST100 (make-node 100 "large" BST10 false))

#; 
(define (fn-for-bst lst)
  (cond [(false? lst) (...)]
        [else
          (... (node-key lst)
               (node-val lst)
               (fn-for-bst (node-l lst))
               (fn-for-bst (node-r lst)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic-distinct: false
;; - compound: (make-node Integer String BST BST)
;; - self-reference: (node-l lst) has type BST
;; - self-reference: (node-r lst) has type BST

;; Functions:

;; BST -> Image
;; produce SIMPLE rendering of BST
;; ASSUME BST is relatively well balanced
(check-expect (render_bst false) MTTREE)
(check-expect (render_bst BST1)
              (above (render_key_val 1 "abc" )
                     (lines (image-width (render_bst false))
                            (image-width (render_bst false)))
                     (beside (render_bst false)
                             (render_bst false))))
; <template from BST>

(define (render_bst bst)
  (cond [(false? bst) MTTREE] 
        [else
          (above (render_key_val (node-key bst) (node-val)))]))
