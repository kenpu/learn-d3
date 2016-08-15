(ns imdb.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [imdb.graph :as graph]
            [imdb.data :as d]
            [imdb.d3-canvas :as d3canvas]
            [imdb.scatter :as scatter]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(enable-console-print!)

(def source js/genres)
;(def source js/countries)

(defn try-big-array []
  (let [big-array (.split js/IMDB_MOVIE_GENRES "|")
        small-array (.slice big-array (* 3 8000) (* 3 8010))]
    (println small-array)))

(defn get-location []
  (if-let [location (-> js/window (aget "location") (aget "search"))]
    (keyword (subs location 1))
    :unknown))


; ===============================================
; Tasks
; ===============================================

(defn FORCE-LAYOUT []
  (let [data          source
        edges         (-> data .-sim js->clj)
        mst           (graph/kruskal edges)
        [nodes links] (d/mk-d3-data data mst)]
    (d3canvas/force-layout nodes links)))

(defn SCATTER []
  (let [{:keys [nodes links]} (d/retrieve!)]
    (scatter/scatter nodes links)))




(defn DEBUG [loc]
  (let [data (d/retrieve!)]
    (js/console.debug (clj->js data))
    (scatter/scatter (:nodes data) (:links data))))

(defn UNKNOWN [loc]
  (-> "body" js/$ (.empty) (.append (str "Unknown: " loc))))

(defn main []
  (let [loc (get-location)]
    (case loc
      :debug (DEBUG loc)
      :layout (FORCE-LAYOUT)
      :scatter (SCATTER)
      (UNKNOWN loc))))


(main)
