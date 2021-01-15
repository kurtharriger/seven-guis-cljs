(ns seven-guis.app
  (:require
   [reagent.core :as reagent :refer [atom cursor]]
   [reagent.dom :as rdom]))

;; Counter
(defn new-counter-state [] 0)
(defn counter [count]
  (fn []
    [:div.component.counter
     [:h1 "Counter"]
     [:button {:on-click #(swap! count inc)} "Increment"]
     [:span.count @count]
     ]))

;; Temp Converter
(defn new-temp-state [] [0 :celsius])

(defn get-temp-c [[value unit]]
  (case unit
    :celsius value
    :fahrenheit (/ (* (- value 32) 5) 9)))

(defn get-temp-f [[value unit]]
  (case unit
    :celsius (+ (/ (* value 9) 5) 32)
    :fahrenheit value))

(defn temp-converter [temp]
  (fn []
    [:div.component.temp-converter
     [:h1 "Temp Converter"]
     [:div
      [:div [:label "Celsius:"]
       [:input {:type :text :value (get-temp-c @temp) :on-change #(reset! temp [(-> % .-target .-value) :celsius])}]]
      [:div [:label "Fahrenheit:"]

       [:input {:type :text :value (get-temp-f @temp) :on-change #(reset! temp [(-> % .-target .-value) :fahrenheit])}]]]]))

;; App
(defonce state (atom {:counter (new-counter-state)
                      :temp (new-temp-state)}))
(defn app []
  [:div
   [counter (cursor state [:counter])]
   [temp-converter (cursor state [:temp])]])

(defn ^:dev/after-load init []
  (rdom/render [app] (.getElementById js/document "root")))
