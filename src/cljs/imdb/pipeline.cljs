(ns imdb.pipeline
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(defn pipeline
  []
  (let [c1 (chan)
        c2 (chan)]
    (go-loop []
             (println "Waiting for notification... <! c2")
             (let [ping (<! c2)]
               (println "Anealing... >! c1:", ping)
               (>! c1 {:message "Do nothing"})
               (recur)))
    [c1 c2]))
