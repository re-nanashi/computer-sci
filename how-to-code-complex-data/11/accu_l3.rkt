;; Data definitions:

(define-struct wiz (name house kids))
;; Wizard is (make-wiz String String (listof Wizard))

(define Wa (make-wiz "A" "S" empty))
(define Wb (make-wiz "B" "G" empty))
(define Wc (make-wiz "C" "R" empty))
(define Wd (make-wiz "D" "H" empty))
(define We (make-wiz "E" "R" empty))
(define Wf (make-wiz "F" "R" (list Wb)))
(define Wg (make-wiz "G" "S" (list Wa)))
(define Wh (make-wiz "H" "S" (list Wc Wd)))
(define Wi (make-wiz "I" "H" empty))
(define Wj (make-wiz "J" "R" (list We Wf Wg)))
(define Wk (make-wiz "K" "G" (list Wh Wi Wj)))

#; ;template, arb-arity tree, encapsulated w/ local
(define (fn-for-wiz w)
  (local [(define (fn-for-wiz w)
            (... (wiz-name w)
                 (wiz-house w)
                 (fn-for-low (wiz-kids w))))
          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                    (... (fn-for-wiz (first low))
                         (fn-for-low (rest low)))]))]
    (fn-for-wiz w)))

(define-struct cp (child parent))
;; ChildParentPair is (make-cp String String)


;; Functions:

;; Wizard -> (listof String)
;; given a wizard, produce the names of every wizard that is placed 
;; in the same house as their immediate parent.
(check-expect (immediate_child We) empty)
(check-expect (immediate_child Wg) (list "A"))
(check-expect (immediate_child Wk) (list "E" "F" "A"))

#; ;first try
(define (immediate_child w0)
  (local [(define (fn-for-wiz w rest_list ans)
            (local [(define create_cp_list 
                      (map (lambda (x) (make-cp x (wiz-house (cp-child w)))) (wiz-kids (cp-child w))))]
              (if (string=? (wiz-house (cp-child w)) (cp-parent w))
              (fn-for-low (append rest_list create_cp_list)
                          (append ans (list (wiz-name (cp-child w)))))
              (fn-for-low (append rest_list create_cp_list) 
                          ans))))

          (define (fn-for-low low ans)
            (cond [(empty? low) ans]
                  [else
                    (fn-for-wiz (first low) (rest low) ans)]))]
    (fn-for-wiz (make-cp w0 "") empty empty)))

;; Second try
(define (immediate_child w0)
  (local [(define (fn-for-wiz w rest_list ans)
            (fn-for-low (append rest_list 
                                (map (lambda (x) (make-cp x (wiz-house (cp-child w)))) 
                                     (wiz-kids (cp-child w)))) 
                        (if (string=? (wiz-house (cp-child w)) (cp-parent w)) 
                          (append ans (list (wiz-name (cp-child w))))
                          ans)))

          (define (fn-for-low low ans)
            (cond [(empty? low) ans]
                  [else
                    (fn-for-wiz (first low) (rest low) ans)]))]
    (fn-for-wiz (make-cp w0 "") empty empty)))

#; ; Instructor's solution
(define (same-house-as-parent w)
  (local [(define-struct wle (w ph))
          (define (fn-for-wiz todo w rsf)
            ;; Depth-First traversal
            (fn-for-low (append (map (lambda (x) 
                                       (make-wle x (wiz-house w)))
                                     (wiz-kids w)) 
                                todo)
                        (if (string=? (wiz-house w) ph)
                          (cons (wiz-name w) rsf)
                          rsf)))

          (define (fn-for-low todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                    (fn-for-wiz (rest todo)
                                (wle-w (first todo))
                                (wle-ph (first todo))
                                rsf)]))]
    (fn-for-wiz empty w "" empty)))

#; ;Solution for lost context accumulator
(define (immediate_child w0)
  (local [(define (fn-for-wiz w acc)
            (if (string=? (wiz-house w) acc)
              (cons (wiz-name w)
                    (fn-for-low (wiz-kids w) (wiz-house w)))
              (fn-for-low (wiz-kids w) (wiz-house w))))

          (define (fn-for-low low acc)
            (cond [(empty? low) empty]
                  [else
                    (append (fn-for-wiz (first low) acc) 
                            (fn-for-low (rest low) acc))]))]
    (fn-for-wiz w0 "")))

;; Wizard -> Natural
;; given a wizard, produce the number of wizards in the tree
(check-expect (number_of_wizards We) 1)
(check-expect (number_of_wizards Wk) 11)
(check-expect (number_of_wizards Wj) 6)

#; ;not tail recursive accumulator since both functions should have accumulators
(define (number_of_wizards w0)
  (local [(define (fn-for-wiz w)
            (fn-for-low (wiz-kids w) 1))
          (define (fn-for-low low acc)
            (cond [(empty? low) acc]
                  [else 
                    (fn-for-low (rest low) 
                                (+ acc (fn-for-wiz (first low))))]))]
    (fn-for-wiz w0)))

#; ;Breadth-First Traversal
(define (number_of_wizards w0)
  (local [(define (fn-for-wiz w rest_list cnt)
            ;; rest_list is first to not go down the tree 
            (fn-for-low (append rest_list (wiz-kids w)) 
                        (add1 cnt)))
          (define (fn-for-low low cnt)
            (cond [(empty? low) cnt]
                  [else 
                    (fn-for-wiz (first low) (rest low) cnt)]))]
    (fn-for-wiz w0 empty 0)))

; Depth-First Traversal
(define (number_of_wizards w0)
  (local [(define (fn-for-wiz w rest_list cnt)
            (fn-for-low (append (wiz-kids w) rest_list)
                        (add1 cnt)))
          (define (fn-for-low low cnt)
            (cond [(empty? low) cnt]
                  [else
                    (fn-for-wiz (first low) (rest low) cnt)]))]
    (fn-for-wiz w0 empty 0)))
