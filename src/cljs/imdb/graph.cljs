(ns imdb.graph
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [clojure.set :refer [union]]
            [tailrecursion.priority-map :refer [priority-map priority-map-by]]))

;; creates a graph by extracting the first node
;; from each edge.
;; The resulting graph is a hash-map
;; of the form
;; { <source> [[<source> <target> <sim>] ...] }
(defn mk-graph [edges]
  (group-by first edges))

;; constructs the vertices from edges
(defn get-vertices [edges]
  (set (mapcat #(subvec % 0 2) edges)))

;; FIND-UNION methods

;; makes singletone sets
(defn make-sets [vertices]
  (map #(set [%]) vertices))

;; finds the sets
(defn find-set [sets u]
  (let [found (filter #(contains? % u) sets)]
    (if (empty? found) nil (first found))))

;; merge union
(defn merge-union [sets set-u set-v]
  (let [others (remove #(or (= %1 set-u) (= %1 set-v)) sets)
        set-uv (union set-u set-v)]
    (conj others set-uv)))

;; compute the minimal spanning tree using
;; the kruskal algorithm

(defn kruskal [edges]
  (loop [tree-edges []
         edges      (->> edges (sort-by #(nth % 2)) reverse)
         sets       (make-sets (get-vertices edges))]
    (cond
      (empty? edges) tree-edges
      :else (let [[u v sim :as edge] (first edges)
                  edges     (rest edges)
                  set-u     (find-set sets u)
                  set-v     (find-set sets v)]
              (if (= set-u set-v)
                (recur tree-edges edges sets)
                (let [tree-edges (conj tree-edges [u v])
                      sets       (merge-union sets set-u set-v)]
                  (println "connecting" u v sim)
                  (recur tree-edges edges sets)))))))

;;
;; create a look up table
;;
(defn lookup-table
  [edges]
  (loop [edges edges
         table {}]
    (if (empty? edges)
      table
      (let [[u v sim] (first edges)]
        (recur (rest edges) (assoc-in table [u v] sim))))))

(defn lookup-fn [edges]
  (let [table (lookup-table edges)]
    (fn [u v]
      (get-in table [u v]))))


