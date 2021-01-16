(ns seven-guis.app
  (:require
   [reagent.core :as reagent :refer [atom cursor]]
   [reagent.dom :as rdom]
   [reitit.frontend :as rf]
   [reitit.frontend.easy :as rfe]))

(defn target-value [event]
  (-> event .-target .-value))

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
       [:input {:type :text :value (get-temp-c @temp) :on-change #(reset! temp [(target-value %) :celsius])}]]
      [:div [:label "Fahrenheit:"]
       [:input {:type :text :value (get-temp-f @temp) :on-change #(reset! temp [(target-value %) :fahrenheit])}]]]]))

;; Flight Booker

(defn new-flight-booker-state [] {:type :one-way
                                  :departure (.toLocaleDateString (js/Date.))
                                  :return nil})

(defn validate-date [str]
  (if str
    (let [date (js/Date. str)]
      (if (js/isNaN date) :invalid date))
    :invalid))

(def invalid? #(= :invalid %))
(def valid? (complement invalid?))


(defn flight-booker [state]
  (let [{:keys [type departure return]} @state
        departure-date (validate-date departure)
        return-date    (validate-date return)
        return-date    (if (and
                             (= type :return)
                             (and (valid? return-date) (not (nil? return-date)))
                             (valid? departure-date)
                             (> (.getTime return-date) (.getTime departure-date)))
                         return-date :invalid)
        return-date    (if (= type :return) return-date)
        invalid        (some invalid? [departure-date return-date])]
    (prn return-date)
    [:div.component.flight-booker
     [:h1 "Flight Booker"]
     [:form {:on-submit (fn [event]
                          (.preventDefault event)
                          (js/alert (str (name type) " " (.toLocaleDateString departure-date) " " (if return-date (.toLocaleDateString return-date)))))}
      [:label "Type"]
      [:select {:on-change #(swap! state assoc :type (-> (target-value %) keyword))}
       [:option {:value :one-way} "One-way flight"]
       [:option {:value :return} "Return flight"]]
      [:label "Departure Date"]
      [:input {:type "text"
               :value departure
               :class (if (= :invalid departure-date) :invalid)
               :on-change #(swap! state assoc :departure (target-value %))}]
      [:div {:class (if (= type :one-way) :disabled nil)}
       [:label "Return Date"]
       [:input {:type "text"
                :disabled (boolean (= type :one-way))
                :value (if (= type :return) return)
                :class (if (= :invalid return-date) :invalid)
                :on-change #(swap! state assoc :return (target-value %))}]]
      [:input {:type "submit"
               :disabled (boolean invalid)
               :value "Book Now"}]]]
    )
  )

;; Timer

(defn new-timer-state []
  {:start-time (.now js/Date)
   :duration (* 60 1000)})

(defn percent-complete [{:keys [start-time duration]}]
  (let [elapsed (min duration (- (.now js/Date) start-time))
        pc      (/ elapsed duration)]
    pc))

(defn progress-bar [percent]
  (fn [] [:div.progress
          [:div.progress-bar {:style {:width (str (* 100 @percent) "%")}}]
          [:span (str (int (* 100 @percent))) "%"]
          ]))

(defn timer [state]
  (let [pc (atom (percent-complete @state))]
    (prn :once)
    (do (reagent/next-tick (fn update []
                             (reset! pc (percent-complete @state))
                             (reagent/next-tick update))))
    (fn []
      ;;(prn @pc)
      [:div.component.timer
       [:h1 "Timer"]
       [progress-bar pc]
       [:br]
       [:input {:type "range" :min 1 :max (* 5 60 1000)
                :value (:duration @state)
                :on-change #(swap! state assoc :duration (target-value %))}]
       [:span (str (int (/ (:duration @state) 1000)) "s")]
       [:br]
       [:button {:on-click #(swap! state assoc :start-time (.now js/Date))} "Reset"]

       ])))

;; Router

(def routes
  [["/" nil]
   ["/counter" ::counter]
   ["/temp-converter" ::temp-converter]
   ["/flight-booker" ::flight-booker]
   ["/timer" ::timer]])

(def page-for
  {::counter #'counter
   ::temp-converter #'temp-converter
   ::flight-booker #'flight-booker
   ::timer #'timer
   })

(defn navbar []
  (fn []
    [:ul.nav
     [:li [:a {:href (rfe/href ::counter)} "Counter"]]
     [:li [:a {:href (rfe/href ::temp-converter)} "Temp Converter"]]
     [:li [:a {:href (rfe/href ::flight-booker)} "Fight Booker"]]
     [:li [:a {:href (rfe/href ::timer)} "Timer"]]
     ]))

(defn current-page [state]
  (rfe/start!
    (rf/router routes)
    (fn [match]
      (swap! state assoc :page (get-in match [:data :name]))
      )
    {:use-fragment true})
  (fn []
    [:div
     [navbar]
     (if-let [page (:page @state)]
       [(page-for page) (cursor state [page])])]))

(defonce state (atom {::counter (new-counter-state)
                      ::temp-converter (new-temp-state)
                      ::flight-booker (new-flight-booker-state)
                      ::timer (new-timer-state)}))

(defn ^:dev/after-load init []
  (rdom/render [current-page state] (.getElementById js/document "root")))
