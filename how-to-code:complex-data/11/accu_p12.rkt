(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))
(define BT)

;; Natural BT -> Boolean
;; produce true if the tree contains the key
(check-expect (contains? 1 BT1) false)
(check-expect (contains? 1 BT2) true)
(check-expect (contains? 4 BT2) true)
(check-expect (contains? 6 BT2) true)
(check-expect (contains? 2 BT2) false)


(define (contains? k bt)
  (local [(define (fn-for-bt bt wl rsf)
            ;; cond at first 
            (if (not (false? bt))
              (fn-for-st (append (filter (lambda (x) (not (false? x))) (list (node-l bt) (node-r bt))) wl)
                       (or (equal? k (node-k bt)) rsf))
              rsf))

          (define (fn-for-st st rsf)
            (cond [(empty? st) rsf]
                  [else
                    (fn-for-bt (first st) (rest st) rsf)]))]
    (fn-for-bt bt empty false)))

#; Non-tail recursive 
(define (contains? k bt)
  (cond [(false? bt) false]
        [else
          (or (= (node-k bt) k) 
              (contains? k (node-l bt))
              (contains? k (node-r bt)))]))

#; 
(define (contains? k bt)
  (local [(define (contains/one? bt todo)
            (cond [(false? bt) (contains/list? todo)]
                  [else
                    ;; in tail position or so do not use
                    (or (= (node-k bt) k)
                        (contains/one? (node-l bt)
                                        (cons (node-r bt) todo)))]))
          (define (contains/list? todo)
            (cond [(empty? todo) false]
                  [else
                    (contains/one? (first todo) (rest todo))]))]
    (contains/one? bt empty)))

#;
; <template from BT + accumulator>
(define (contains? k bt)
  (local [(define (contains/one? bt todo)
            (cond [(false? bt) (contains/list? todo)]
                  [else
                    (if (= (node-k bt) k)
                      true
                      (contains/one? (node-l bt)
                                     (cons (node-r bt) todo)))]))
          (define (contains/list? todo)
            (cond [(empty? todo) false]
                  [else
                    (contains/one? (first todo) (rest todo))]))]
    (contains/one? bt empty)))

#; ;alternative
(define (contains? k bt)
  (local [(define (contains/one? bt todo)
            (cond [(false? bt) (contains/list? todo)]
                  [else
                    (if (= (node-k bt) k)
                      true
                      (contains/list? (cons (node-l bt)
                                            (cons (cons (node-r bt)
                                                        todo)))))]))
          (define (contains/list? todo)
            (cond [(empty? todo) false]
                  [else
                    (contains/one? (first todo) (rest todo))]))]
    (contains/one? bt empty)))
