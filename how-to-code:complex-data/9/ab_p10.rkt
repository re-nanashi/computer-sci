(require racket/file)

(define-struct weather (date max_tmp avg_tmp min_tmp rain snow precip))
;; Weather is (make-weather String Number Number Number Number Number Number )
;; interp. Data about weather in Vancouver on some date
;; (make-weather date mat at mit r s p) means that
;; one the day indicated by date:
;; - the maximum temperature in Celsius was mat
;; - the average temperature in Celsius was at
;; - the minimum temperature in Celsius was nt
;; - r millimeters of rain fell
;; - s millimeters of snow fell
;; - p millimeters of total precipication fell

(define W0 (make-weather "7/2/88" 21.9 17.7 13.4 0.2 0 0.2))
(define W1 (make-weather "7/3/88" 25.9 10 13.4 0.2 0 0.2))
(define W2 (make-weather "7/4/88" 25.9 20 13.4 0.2 0 0.2))
(weather-avg_tmp W0)

(define (fn-for-weather w)
  (... (weather-date w)
       (weather-max_tmp w)
       (weather-avg_tmp w)
       (weather-min_tmp w)
       (weather-rain w)
       (weather-snow w)
       (weather-precip w)))

;; ListOfWeather is one of:
;; - empty
;; - (cons Weather ListOfWeather)
;; interp. A list of weather data
(define LOW0 empty)
(define LOW1 (list W0))
(define LOW2 (list W0 W1))
(define LOW3 (list W0 W1 W2))

#;
(define (fn-for-low low)
  (cond [(empty? low) (...)]
        [else
          (... (fn-for-weather (first low))
               (fn-for-low (rest low)))]))

(define WEATHER_DATA
  (local [(define (data->weather d)
            (make-weather (first d) (second d) (third d) (fourth d)
                          (fifth d) (sixth d) (seventh d)))]
    (map data->weather (file->value "weather.ss"))))

;; ListOfWeather -> Number
;; produce the total sum of rainfall in millimeters on days where the 
;; avg_tmp was greater than 15 degrees Celsius.
(check-expect (sum LOW0) 0)
(check-expect (sum LOW1) 0.2)
(check-expect (sum LOW2) 0.2)
(check-expect (sum LOW3) (* 0.2 2))
(check-expect (sum WEATHER_DATA) 2545.3)

(define (sum low)
  (local [(define (pred w)
            (> (weather-avg_tmp w) 15))
          (define (fn w)
            (weather-rain w))]
    (foldr + 0 (map fn (filter pred low)))))
