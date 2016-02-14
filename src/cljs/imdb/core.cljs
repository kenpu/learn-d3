(ns imdb.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [imdb.graph :as graph]
            [imdb.d3-canvas :as d3canvas]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(enable-console-print!)

(let [edges (-> js/genres .-sim js->clj)]
  (let [mst (graph/kruskal edges)]
    (.start (d3canvas/force-layout edges mst d3canvas/tick))))
