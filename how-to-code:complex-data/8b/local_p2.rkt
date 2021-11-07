(require 2htdp/image)

;; Constants

(define TEXT_SIZE 14)
(define TEXT_COLOR "BLACK")

(define KEY_VAL_SEPARATOR ":")

(define MTTREE (rectangle 20 1 "solid" "white"))


;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;; - false
;; - (make-node Integer String BST BST)
;; interp. false no BST, or empty BST
;;         key is the node key        
;;         val is the node val        
;;         l and r are left and right subtress
;; INVARIANT: for a given node:
;;         key is > all keys in its l child
;;         key is < all keys in its r child
;;         the same key never appears twice in the tree
(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false)) 
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42 
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100 
  (make-node 100 "large" BST10 false))

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
;; - atomic-distinct: false
;; - compound: (make-node Integer String BST BST)
;; - self-reference: (node-l t) has type BST
;; - self-reference: (node-r t) has type BST

;; BST -> Image
;; produce SIMPLE rendering of bst
;; ASSUME BST is relatively well balanced
(check-expect (render_bst false) MTTREE)
(check-expect (render_bst BST1) 
              (above (render_key_val 1 "abc")
                     (lines (image-width (render_bst false))
                            (image-width (render_bst false)))
                     (beside (render_bst false)
                             (render_bst false))))

(define (render_bst bst)
  (cond [(false? bst) MTTREE]
        [else
          (local [
                  (define render_lbst (render_bst (node-l bst)))
                  (define render_rbst (render_bst (node-r bst)))
                  (define get_image_widthl (image-width render_lbst)) 
                  (define get_image_widthr (image-width render_rbst))] 
            (above (render_key_val (node-key bst) (node-val bst))
                 (lines get_image_widthl
                        get_image_widthr)
                 (beside render_lbst
                         render_rbst)))]))

;; Integer String -> Image
;; render key and value to form the body of a node
(check-expect (render_key_val 99 "foo") 
              (text (string-append "99" KEY_VAL_SEPARATOR "foo") TEXT_SIZE TEXT_COLOR))
(define (render_key_val k v)
  (text (string-append (number->string k)
                       KEY_VAL_SEPARATOR
                       v)
        TEXT_SIZE
        TEXT_COLOR))

;; Natural Natural -> Image
;; produce lines to l/r subtrees based on width of those subtrees
(check-expect (lines 60 130)
              (add-line (add-line (rectangle (+ 60 130) (/ 190 4) "solid" "white")
                                  (/ (+ 60 130) 2) 0
                                  (/ 60 2)         (/ 190 4)
                                  "black")
                        (/ (+ 60 130) 2) 0
                        (+ 60 (/ 130 2)) (/ 190 4)
                        "black"))

(define (lines lw rw)
  (add-line (add-line (rectangle (+ lw rw) (/ (+ lw rw) 4) "solid" "white")  ;background
                      (/ (+ lw rw) 2)  0
                      (/ lw 2)         (/ (+ lw rw) 4)
                      "black")
            (/ (+ lw rw) 2)  0
            (+ lw (/ rw 2))  (/ (+ lw rw) 4)
            "black"))

(define BSTA (make-node 100 "A" BST10 BST10))
(define BSTB (make-node 101 "B" BSTA BSTA))
(define BSTC (make-node 102 "C" BSTB BSTB))
(define BSTD (make-node 103 "D" BSTC BSTC))
(define BSTE (make-node 104 "E" BSTD BSTD))
(define BSTF (make-node 104 "E" BSTE BSTE))

(time (rest (list (render_bst BSTA))))
(time (rest (list (render_bst BSTB))))
(time (rest (list (render_bst BSTC))))
(time (rest (list (render_bst BSTD))))
(time (rest (list (render_bst BSTE))))
