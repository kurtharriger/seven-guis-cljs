(ns seven-guis.app
  (:require
    [reagent.core :as reagent :refer [atom cursor]]
    [reagent.dom :as rdom]))

(defn counter [count]
  (fn []
    [:div.component.counter
     [:h1 "Counter"]
     [:button {:on-click #(swap! count inc)} "Increment"]
     [:span.count @count]
     ]))

(defonce state (atom {:counter 0}))
(defn app []
  [counter (cursor state [:counter])])

(defn ^:dev/after-load init []
  (rdom/render [app] (.getElementById js/document "root")))
