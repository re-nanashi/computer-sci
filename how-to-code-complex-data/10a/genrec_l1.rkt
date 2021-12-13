(require 2htdp/image)

(define CUTOFF 2)

;; Number -> Image
;; produce a Sierpinski Triangle of the given size
(check-expect (stri CUTOFF) (triangle CUTOFF "outline" "red"))
(check-expect (stri (* CUTOFF 2)) 
              (overlay (triangle (* CUTOFF 2) "outline" "red")
                       (local [(define sub (triangle CUTOFF "outline" "red"))]
                         (above sub
                                (beside sub sub)))))

; (define (stri s) (square 0 "solid" "white")) ; stub

#;
(define (genrec-fn d)
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d 
              (genrec-fn (next-problem d)))]))

(define (stri s)
  (cond [(<= s CUTOFF) (triangle s "outline" "red")]
        [else
          (overlay (triangle s "outline" "red")
                   (local [(define sub (stri (/ s 2)))]
                     (above sub
                            (beside sub sub))))]))
;; Number -> Image
;; produce a Sierpinski Carpet of the given size
(check-expect (scar CUTOFF) (square CUTOFF "outline" "red"))
(check-expect (scar (* 3 CUTOFF)) 
              (overlay (square (* 3 CUTOFF) "outline" "red")
                       (local [(define sub (square CUTOFF "outline" "red"))
                               (define blk (square CUTOFF "solid" "white"))]
                         (above (beside sub sub sub)
                                (beside sub blk sub)
                                (beside sub sub sub)))))

#;
(define (genrec-fn d)
  (cond [(trivial? d) (trivial-answer d)]
        [else
         (... d 
              (genrec-fn (next-problem d)))]))

(define (scar s)
  (cond [(<= s CUTOFF) (square s "outline" "red")]
        [else
          (overlay (square s "outline" "red")
                   (local [(define sub (scar (/ s 3)))
                           (define blk (square (/ s 3) "solid" "white"))]
                     (above (beside sub sub sub)
                            (beside sub blk sub)
                            (beside sub sub sub))))]))
