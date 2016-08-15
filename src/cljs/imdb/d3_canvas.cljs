(ns imdb.d3-canvas
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [imdb.draw :refer [cnf state resolve!]]
            [imdb.graph :as graph]
            [imdb.data :as d]
            [imdb.pipeline :refer [pipeline]]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(defn finalize [{:keys [nodes links]}]
  (d/save! nodes links)
  (js/alert "Done"))

;; creates a force layout
;; It uses the d3.layout
(defn force-layout
  [nodes links]
  (let [[<<driver c2] (pipeline)
        layout (.force d3.layout)
        pingback #(do (js/console.debug "M pingback")
                      (put! c2 "Ping"))]
    (put! c2 "Init")
    (go-loop [i 0]
             (let [{:keys [message init tick] :as stage} (<! <<driver)
                   callback (fn [e] 
                              (when tick (tick {:message message
                                                :nodes nodes 
                                                :links links 
                                                :e e})))]
               (if stage
                 (do (js/console.debug "STAGE [" i "]:" message)
                     (when init (init {:layout layout 
                                       :state state 
                                       :nodes nodes 
                                       :links links 
                                       :counter i}))
                     (when tick
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
                 (do (js/console.debug "M closing c2") 
                     (close! c2)
                     (finalize {:nodes nodes :links links})))))))
