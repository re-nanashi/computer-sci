;; lookup

(define-struct account (num name))
;; Accounts is one of:
;; - empty
;; - (cons (make-account Natural String) Accounts)
;; interp. a list of accounts, where each
;;          num is an account number,
;;          name is the person's name
(define ACS1 empty)
(define ACS2 
  (list (make-account 1 "abc") (make-account 4 "dcj") (make-account 3 "ilk") (make-account 7 "ruf")))

#;
(define (fn-for-accounts accs)
  (cond [(empty? accs) (...)]
        [else
          (... (account-num (first accs))               ; Natural
               (account-name (first accs))              ; String
               (fn-for-accounts (rest accs)))]))

;; Accounts Natural -> String or false
;; try to find the account with the given number. If found produce name, else produce false.
(check-expect (lookup ACS1 1) false)
(check-expect (lookup ACS2 1) "abc") 
(check-expect (lookup ACS2 2) false) 
(check-expect (lookup ACS2 3) "ilk") 

(define (lookup accs n)
  (cond [(empty? accs) false]
        [else
          (if (equal? (account-num (first accs)) n) 
            (account-name (first accs)) 
            (lookup (rest accs) n))]))
