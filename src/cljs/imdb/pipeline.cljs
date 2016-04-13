(ns imdb.pipeline
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(defn pipeline
  []
  (let [c1 (chan)
        c2 (chan)]
    (go-loop [n 0]
             (println "[PIPELINE] Waiting for notification... <! c2")
             (let [ping (<! c2)]
               (println "[PIPELINE] Received" ping ". Anealing... >! c1.")
               (>! c1 {:message "Do nothing"})
               (recur (inc n))))
    [c1 c2]))
