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
    (do (reagent/next-tick (fn update []
                             (reset! pc (percent-complete @state))
                             (reagent/next-tick update))))
    (fn []
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


;; CRUD

(defn empty-crud-state []
  {:names {}
   :next-id 0
   :selected nil
   :filter nil
   :first nil
   :last nil})

(defn add-name [{:keys [names next-id] :as state} first last]
  (assoc state
    :names (assoc names next-id {:first first :last last})
    :next-id (inc next-id)
    :selected nil
    :filter nil
    :first nil
    :last nil))

(defn update-name [{:keys [names selected] :as state} first last]
  (if selected
    (assoc state
      :names (assoc names selected {:first first :last last}))
    state))

(defn delete-name [{:keys [names selected] :as state}]
  (-> state
    (update :names dissoc selected)
    (assoc :name nil selected :first nil :last nil :selected nil)))

(defn new-crud-state []
  (->
    (empty-crud-state)
    (add-name "James" "Doe")
    (add-name "John" "Jackson")
    (add-name "Betty" "Moss")))

(defn filtered-names [names search]
  (if search
    (->> names
      (map (fn [row]
             (let [[id {:keys [first last]}] row
                   text (str last ", " first)]
               (if (re-find (re-pattern search) text) id))))
      (remove nil?)
      (vec))
    (keys names)))

(defn crud [state]
  (fn []
    (let [{:keys [names selected filter first last]} @state
          filtered-list (filtered-names names filter)]
      [:div.component.crud
       [:input {:type "text"
                :value filter
                :on-change #(swap! state assoc :filter (target-value %))}]
       [:ul
        (for [id filtered-list
              :let [{:keys [first last]} (get names id)]]
          ^{:key id} [:li
                      {:class (if (= selected id) :selected)
                       :on-click #(swap! state assoc :selected id :first first :last last)}
                      (str last ", " first)])
        ]
       [:input {:type "text"
                :value first
                :on-change #(swap! state assoc :first (target-value %))}]
       [:input {:type "text"
                :value last
                :on-change #(swap! state assoc :last (target-value %))}]
       [:div
        [:button
         {:disabled
          (or (empty? first) (empty? last))
          :on-click #(swap! state add-name first last)}
         "Create"]
        [:button
         {:disabled (or (nil? selected) (empty? first) (empty? last))
          :on-click #(swap! state update-name first last)}
         "Update"]
        [:button
         {:disabled (not selected)
          :on-click #(swap! state delete-name)}
         "Delete"
         ]]]))
  )

;; Router

(def routes
  [["/" nil]
   ["/counter" ::counter]
   ["/temp-converter" ::temp-converter]
   ["/flight-booker" ::flight-booker]
   ["/timer" ::timer]
   ["/crud" ::crud]])

(def page-for
  {::counter #'counter
   ::temp-converter #'temp-converter
   ::flight-booker #'flight-booker
   ::timer #'timer
   ::crud #'crud
   })

(defn navbar []
  (fn []
    [:ul.nav
     [:li [:a {:href (rfe/href ::counter)} "Counter"]]
     [:li [:a {:href (rfe/href ::temp-converter)} "Temp Converter"]]
     [:li [:a {:href (rfe/href ::flight-booker)} "Fight Booker"]]
     [:li [:a {:href (rfe/href ::timer)} "Timer"]]
     [:li [:a {:href (rfe/href ::crud)} "CRUD"]]
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
                      ::timer (new-timer-state)
                      ::crud (new-crud-state)}))

(defn ^:dev/after-load init []
  (rdom/render [current-page state] (.getElementById js/document "root")))
