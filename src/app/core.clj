(ns app.core
  (:import (java.time LocalDate)
           (java.time.temporal ChronoUnit))
  (:require [distributions.core :refer [sample normal mean]]
            [oz.core :as oz]))


(defn gaussian []
  (sample (normal 0 1)))

(comment
  (repeatedly 5 gaussian))

(defn gbm-step [price dt rate volatility]
  (let [drift (* price rate dt)
        shock (* price volatility (Math/sqrt dt) (gaussian))
        change (+ drift shock)]
    (+ price change)))

(comment
  (take 100 (iterate #(gbm-step % 1/365 0.01 0.15) 257)))

(defn make-sample-outcome [name days]
  (let [sample (take days (iterate #(gbm-step % 1/365 0.01 0.15) 257))
        annotate (fn [x y] {:time x :price y :item name})
        annotated (map annotate (range) sample)]
    annotated))

(defn plot-data []
  (let [values (mapcat #(make-sample-outcome % 100) (map #(str "sample " %) (range 10)))]
  {:data {:values values}
   :encoding {:x {:field :time :type :quantitative}
              :y {:field :price :type :quantitative}
              :color {:field :item :type :nominal}}
   :mark :line}))

(comment
  (oz/start-server!)
  (oz/view! (plot-data)))


(defn call-option-value [price-at-expiration strike-price]
  (Math/max (- price-at-expiration strike-price) 0.0))

(comment
  (call-option-value 360.0 280.0)
  (call-option-value 10.0 280.0))


(defn year-fraction-until [date]
  (/ (. ChronoUnit/DAYS between (LocalDate/now) date) 365))

(comment
  (year-fraction-until (LocalDate/now))
  (year-fraction-until (. (LocalDate/now) (plusDays 180)))

(defn present-value [value rate expiration]
  (* (Math/exp (- (* rate expiration))) value))

(comment
  (present-value 100 0.07 0)
  (present-value 100 0.07 0.5)
  (present-value 100 0.07 1))


(defn simulate-outcome [price strike rate volatility expiration]
  (let [steps 100
        dt (/ expiration steps)
        prices (iterate #(gbm-step % dt rate volatility) price)
        price-at-expiration (last (take steps prices))]
    (call-option-value price-at-expiration strike)))

(comment
  (repeatedly 5 #(simulate-outcome 1924 1925 0.01 0.45 0.5)))


(defn evaluate-call-option [& {:keys [security-price strike-price risk-free-rate volatility expiration]}]
  (let [expiration (year-fraction-until expiration)
        simulate-fn (partial simulate-outcome security-price strike-price risk-free-rate volatility expiration)
        n 1000]
    (-> (repeatedly n simulate-fn) mean (present-value risk-free-rate expiration))))

(comment
  (evaluate-call-option
    :security-price 65.24
    :strike-price 65
    :risk-free-rate 0.01
    :volatility 0.3955
    :expiration (LocalDate/of 2020 4 14)))
