(ns imdb.d3-canvas
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [imdb.draw :refer [cnf state resolve!]]
            [imdb.graph :as graph]
            [imdb.pipeline :refer [pipeline]]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))


;; creates a force layout
;; It uses the d3.layout
(defn force-layout
  [nodes links]
  (let [[c1 c2] (pipeline)
        layout (.force d3.layout)]
    (put! c2 "Init")
    (go-loop [i 0]
             (let [{:keys [message init tick] :as stage} (<! c1)
                   callback (fn [e] 
                              (when tick (tick {:message message
                                                :nodes nodes 
                                                :links links 
                                                :e e})))
                   pingback #(put! c2 "Ping")]
               (if stage
                 (do (println "STAGE [" i "]:" message)
                     (when init (init {:layout layout 
                                       :state state 
                                       :nodes nodes 
                                       :links links 
                                       :counter i}))
                     (when tick
                       (println "LAYOUT(" (cnf :gravity) (cnf :charge) ")")
                       (.. layout
                           (alpha (cnf :alpha))
                           (gravity (cnf :gravity))
                           (charge  (cnf :charge))
                           (links   links)
                           (linkStrength (cnf :linkStrength))
                           (nodes nodes)
                           (size (cnf :dimension))
                           (on "tick" callback)
                           (on "end" pingback)
                           (start))
                      (recur (inc i))))
                 (close! c2))))))
