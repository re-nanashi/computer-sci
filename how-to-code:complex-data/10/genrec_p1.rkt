(require 2htdp/image)

(define TRIVIAL_SIZE 5)
(define STEP (/ 2 5))

;; Number -> Image
;; produce a circle fractal
(check-expect (draw_leaf TRIVIAL_SIZE) (circle TRIVIAL_SIZE "solid" "lightblue"))

(define (cfrac r)
  (local [(define parent (circle r "solid" "lightblue"))
          (define child (draw_leaf (* r STEP)))]
    (above child
           (beside (rotate 90 child) parent (rotate -90 child))
           (rotate 180 child))))

(define (draw_leaf r)
  (cond [(<= r TRIVIAL_SIZE) (circle r "solid" "lightblue")]
        [else
          (local [(define parent (circle r "solid" "lightblue"))
                  (define child (draw_leaf (* r STEP)))]
            (above child
                   (beside (rotate 90 child) parent (rotate -90 child))))]))
