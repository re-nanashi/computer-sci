;; Data definitions:

(define-struct account (num name))
;; Accounts is one of:
;;  - empty
;;  - (cons (make-account Natural String) Accounts)
;; interp. a list of accounts, where each 
;;           num  is an account number 
;;           name is the person's first name
(define ACS1 empty)
(define ACS2
  (list (make-account 1 "abc") (make-account 4 "dcj") (make-account 3 "ilk")   (make-account 7 "ruf")))
#;
(define (fn-for-accounts accs)
  (cond [(empty? accs) (...)]
        [else
         (... (account-num  (first accs)) ;Natural
              (account-name (first accs)) ;String
              (fn-for-accounts (rest accs)))]))
                                   
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


;; ============
;; Functions:

;; lookup::Accounts Natural -> String or false
;; find the account with given number in accounts. 
;; If found produce the name, else produce false
(check-expect (lookup ACS1 9) false) ; base
(check-expect (lookup ACS2 1) "abc") 
(check-expect (lookup ACS2 4) "dcj") 

;(define (lookup accs n) false)  ; stub
; <template from Accounts>

(define (lookup accs n)
  (cond [(empty? accs) false]
        [else
         (if (equal? (account-num  (first accs)) n)  
           (account-name (first accs)) 
           (lookup (rest accs) n))]))


;; totalNumberOfNodes::BST -> Natural
;; produces the total number of nodes in a tree
(check-expect (totalNumberOfNodes BST0) 0)
(check-expect (totalNumberOfNodes BST1) 1)
(check-expect (totalNumberOfNodes BST4) 2)
(check-expect (totalNumberOfNodes BST3) 4)

;(define (totalNumberOfNodes t) 0) ;stub

(define (totalNumberOfNodes t)
  (cond [(false? t) 0]
        [else
          (+ 1 
             (totalNumberOfNodes (node-l t)) 
             (totalNumberOfNodes (node-r t)))]))

;; sumOfKeys:: BST -> Natural
;; produce the sum of all keys in the BST
(check-expect (sumOfKeys BST0) 0)  ;base
(check-expect (sumOfKeys BST1) 1)  ;base
(check-expect (sumOfKeys BST4) 11)  ;base
(check-expect (sumOfKeys BST3) 15)  ;base

;(define (sumOfKeys bst) 0)      ;stub
; <template from BST>

(define (sumOfKeys t)
  (cond [(false? t) 0]
        [else
          (+ (node-key t) 
             (sumOfKeys (node-l t)) 
             (sumOfKeys (node-r t)))]))

;; getHeight:: BST -> Natural
;; produce the height of a tree
(check-expect (getHeight BST0) 0)       ; base
(check-expect (getHeight BST1) 1)       ; head
(check-expect (getHeight BST4) 2)
(check-expect (getHeight BST3) 3)
(check-expect (getHeight BST10) 4)

;(define (getHeight t) 0)        ; stub
; <template from BST>

(define (getHeight t)
  (cond [(false? t) 0]
        [else
          (+ 1 
             (max (getHeight(node-l t)) 
                  (getHeight(node-r t))))]))

;; insert:: Integer String BST -> BST
;; insert the Interger:String pair into BST
(check-expect (insert 1 "yey" BST0) (make-node 1 "yey" false false)) ; base
(check-expect (insert 8 "hey" BST7) (make-node 7 "ruf" false (make-node 8 "hey" false false)))

;(define (insert key val bst) bst)       ;stub

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
