(require 2htdp/image)
(require 2htdp/universe)

;; Traffic light program

;; ===============
;; Constants:
;; !!!

(define WIDTH 600)
(define HEIGHT 400)

(define COUNTDOWN_MEASUREMENTS 300)
(define COUNTDOWN (square COUNTDOWN_MEASUREMENTS "solid" "black"))

(define TRAFFIC_LIGHT_BODY_WIDTH 200)
(define TRAFFIC_LIGHT_BODY_HEIGHT HEIGHT)
(define TRAFFIC_LIGHT_BODY (rectangle TRAFFIC_LIGHT_BODY_WIDTH TRAFFIC_LIGHT_BODY_HEIGHT "solid" "black"))
(define TRAFFIC_LIGHT (place-image ))

(define CIRCLE_RADIUS 30)
(define MTS (empty-scene WIDTH HEIGHT))

;; ===============
;; Data definitions:

;; TrafficLight is one of:
;; - Number[0, 9]
;; - Number(9, 11]
;; - Number(11, 20]
;; interp. range between color changes
;;   Number[0, 9] is "red"
;;   Number(9, 11] is "yellow"
;;   Number(11, 20] is "green"
(define trafficLight1 9)
(define trafficLight1 20)

#;
(define (trafficLightFunc tl)
  (cond [(and (>= tl 0) (<= tl 9)) (... tl)]
        [(and (> tl 9) (<= tl 11)) (... tl)]
        [(and (> tl 11) (<= tl 20)) (... tl)]))

;; Template rules used:
;; one-of: 3 cases
;; atomic non-distinct: Number[0, 9]
;; atomic non-distinct: Number(9, 11]
;; atomic non-distinct: Number(11, 20]

;; TrafficLight -> TrafficLight 
;; start the world with (main 0)
;; !!!
(define (main tl)
  (big-bang tl                      ; TrafficLight
            (on-tick ... 1)
            (to-draw ...)))

;; TrafficLight -> TrafficLight
;; loop the TrafficLight by incrementing 1 until 20 then back to 0
(check-expect (loop_traffic_light 0) 1)
(check-expect (loop_traffic_light 20) 0)

;(define (loop_traffic_light tl) 0)       ; stub
;; <template from TrafficLight definition>

(define (loop_traffic_light tl)
  (cond [(and (>= tl 0) (< tl 20)) (+ tl 1)]
        [else 0]))

;; TrafficLight -> Image
;; render countdown image and the respective TrafficLight color
;; !!!
(define (render c) MTS)             ; stub

;; render::helper func: TrafficLight Color -> Image
;; render TrafficLight Image 
;; !!!
(define (renderTrafficLight tl color) 
  (place2))

