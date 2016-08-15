(ns imdb.data
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [alandipert.storage-atom :refer [local-storage clear-local-storage!]]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(def storage (local-storage (atom {}) :imdb))

(defn save! [nodes links]
  (swap! storage assoc :nodes nodes :links links)
  (aset js/window "imdb" (clj->js {:nodes nodes :links links})))

(defn retrieve! [] (deref storage))

(defn- percentile
  [nodes]
  (let [total (count nodes)
        sorted (sort-by #(:rawsize %) nodes)]
    (map #(assoc %1 :rank %2 
                    :percentile (/ %2 total))
         sorted (range 1 (inc total)))))

(defn- mk-d3-data

  "Makes the data for D3.
  data: the original data returned from database query
  links: a coll of label pairs, from a spanning tree.
  Returns [vertices, edges] in clj data structures"

  [data links]

  (let [edges (-> data (.-sim) (js->clj))
        sizes (-> data (.-size) (js->clj))
        labels (sort (set (mapcat #(subvec % 0 2) edges)))
        lookup (apply hash-map (interleave labels (range (count labels))))
        nodes (percentile
                (map #(hash-map :label % 
                                :size 3 
                                :rawsize (sizes %)) labels))
        edges (into []
                    (for [[u v] links]
                      {:source (lookup u)
                       :target (lookup v)}))]
    [(clj->js nodes)
     (clj->js edges)]))

(defn get-size [] #js [600 600])
