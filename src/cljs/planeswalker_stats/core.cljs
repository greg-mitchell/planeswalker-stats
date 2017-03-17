(ns planeswalker-stats.core
  (:require
   [planeswalker-stats.common :as pp.common]
   [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(defonce app-state (atom {:text "Hello Chestnut!"}))

(defn greeting []
  [:div
   [:h1 "Planeswalker points"]
   [:p (:text @app-state)]])

(reagent/render [greeting] (js/document.getElementById "app"))
