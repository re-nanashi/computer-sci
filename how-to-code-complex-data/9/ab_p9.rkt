(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD 
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42 
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40 
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))

;; (listof Accounts) (X -> Boolean) X -> (listof Accounts)
;; remove all accounts if pred(param) is true

(define (remove_abstract act pred)
  (local [(define (join act1 act2)
            (cond [(false? act2) act1]
                  [else
                    (make-node (node-id act2) 
                               (node-name act2)
                               (node-bal act2)
                               (join act1 (node-l act2))
                               (node-r act2))]))]
    (cond [(false? act) false]
          [else
            (if (pred act)
              (join (remove_abstract (node-l act) pred)
                    (remove_abstract (node-r act) pred))
              (make-node (node-id act)
                         (node-name act)
                         (node-bal act)
                         (remove_abstract (node-l act) pred)
                         (remove_abstract (node-r act) pred)))]) ))

;; Accounts -> Accounts
;; remove all accounts with a negative balance
(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false)) 
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              false)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors 
               (make-node 4 "Mrs. Doubtfire" -3
                          false 
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))

(define (remove-debtors act)
  (local [(define (pred act)
            (negative? (node-bal act)))]
   (remove_abstract act pred)))

;; Accounts -> Accounts
;; Remove all professors' accounts.  
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false)) 
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false)) false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60 false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))

#;
(define (remove-profs act)
  (cond [(false? act) false]
        [else
         (if (has-prefix? "Prof." (node-name act))
             (join (remove-profs (node-l act))
                   (remove-profs (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-profs (node-l act))
                        (remove-profs (node-r act))))]))

(define (remove-profs act)
  (local [(define (pred act) 
            (has-prefix? "Prof." (node-name act)))]
   (remove_abstract act pred)))

;; String String -> Boolean
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))


;; (listof Accounts) -> (listof Accounts)
;; remove accounts that have odd number of characters on their name.

(define (remove_odd act)
  (local [(define (pred act)
            (odd? (string-length (node-name act))))]
    (remove_abstract act pred)))

;; (Natural String Integer Z Z -> Z) Z Accounts -> Z
;; the fold function for Accounts.

(define (fold_act c b t)
  (cond [(false? t) b]
        [else
          (c (node-id t)
             (node-name t)
             (node-bal t)
             (fold_act (node-l t))
             (fold_act (node-r t)))]))

;; Accounts -> Accounts
;; Deduct 3 CAD monthly fee from every account.

;; <template from fold_act>
(define (charge_fee act)
  (local [(define (c id name balance rl rr)
            (make-node id name (- balance 3) rl rr))]
    (fold_act c false act)))