(ns imdb.pipeline
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [imdb.draw :as draw]))

(def exp #(.exp js/Math %))


(defn- sigmoid
  [x c]
  (/ 1 (+ 1 (exp (- (* c (- x 0.5)))))))

(defn- scale
  [x a b]
  (+ a (* (- b a) x)))

(defn- my-sigmoid
  [x]
  (scale (sigmoid x 10) 5 50))

(defn- rawsize->size
  "Convert the raw size to actual pixel size
  using a non-linear transformation"
  [nodes alpha]
    (doseq [n nodes]
      (let [perc (.-percentile n)
            size (my-sigmoid perc)]
        (aset n "size" (* alpha size)))))

(defn- collapse
  [state frac]
  (let [delta 90]
    (swap! state update :charge + delta)))

(defn- init-fn
  [n]
  (fn [& {:keys [layout state nodes links counter]}]
    (cond
      (< n 3) nil
      (and (>= n 3) (< 7)) (let [frac (/ (- n 3) 3)] 
                             (rawsize->size nodes frac)))
      (and (>= 7) (< 10)) (let [frac (/ (- n 7) 3)]
                            (collapse state frac))
      :else nil))

(defn- get-stage [n]
  (cond
    (<= n 2) {:message "Annealing to remove crossings"
             :init nil
             :tick draw/d3-tick}
    (<= 3 n 6) {:message "Resize categories"
                :init (fn [{:keys [nodes]}]
                        (let [frac (/ (- n 3) 3)]
                          (rawsize->size nodes frac)))
                :tick draw/d3-tick}
    (<= 7 n 9) {:message "Compact all categories"
                :init (fn [{:keys [state]}]
                        (let [frac (/ (- n 7) 3)]
                          (collapse state frac)))
                :tick draw/d3-tick}
    :else nil))


(defn pipeline
  []
  (let [c1 (chan)
        c2 (chan)]
    (go-loop [n 0]
             (println "[PIPELINE] Waiting for notification... <! c2")
             (let [ping (<! c2)]
               (println "[PIPELINE] Received" ping)
               (let [stage (get-stage n)]
                 (if stage 
                   (do (>! c1 stage)
                       (recur (inc n))) 
                   (close! c1)))))
    [c1 c2]))
