(ns imdb.d3-canvas
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [imdb.graph :as graph]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

;; dealing with all of d3 and canvas needs
;;

(defn mk-nodes [edges]
  (clj->js 
    (into [] 
          (map #(hash-map :label %) (graph/get-vertices edges)))))

;; makes the links
;; Links is an array of {:source index :target index}
(defn mk-links 
  [edges tree-edges]
  (let [vertices (clj->js (graph/get-vertices edges))]
    (clj->js
      (into []
            (for [[u v] tree-edges]
              {:source (.indexOf vertices u)
               :target (.indexOf vertices v)})))))

(defn force-layout
  [edges tree-edges tick]
  (let [lookup (graph/lookup-fn edges)
        nodes  (mk-nodes edges)
        callback (fn [e] (tick e nodes))]
    (.. d3.layout 
        (force)
        (gravity 0.05)
        (charge (fn [d i] -30))
        (links (mk-links edges tree-edges))
        (nodes nodes)
        (size #js [800 500])
        (on "tick" callback))))


