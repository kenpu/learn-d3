(ns imdb.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [imdb.graph :as graph]
            [imdb.data :as d]
            [imdb.d3-canvas :as d3canvas]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(enable-console-print!)

(let [data          js/genres
      edges         (-> js/genres .-sim js->clj)
      mst           (graph/kruskal edges)
      [nodes links] (d/mk-d3-data data mst)]
  (d3canvas/force-layout nodes links))
