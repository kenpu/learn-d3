(ns imdb.scatter
  (:require [imdb.data :as d]
            [imdb.draw :as draw]))

(def blue "#33C4FF")
(def red "#f00")
(def hex-radius 5)

(defn- get-cells [hexbin]
  (let [centers (-> hexbin (.centers))]
    (clj->js (into [] (for [c centers]
                        {:x (aget c 0)
                         :y (aget c 1)
                         :i (aget c "i")
                         :j (aget c "j")
                         :radius (dec hex-radius)
                         :color blue})))))

(defn rand-color []
  (if (< (.random js/Math) 0.1) red blue))

(defn rand-255 []
  (int (* 255 (.random js/Math))))
(defn rand-rgb []
  (str "RGB(" (rand-255) "," (rand-255) "," (rand-255)))

(defn colorize-nodes [nodes]
  (let [c (-> js/d3 (.-scale) (.category20))]
    (doseq [n nodes]
      (aset n "color" (c (.-index n))))))

(def exp #(.exp js/Math %))

(defn dist
  "compute the distance of a cell to a node"
  [cell node]
  (let [xc (.-x cell)
        yc (.-y cell)
        xn (.-x node)
        yn (.-y node)
        r  (.-size node)]
    (- (+ (* (- xc xn) (- xc xn)) (* (- yc yn) (- yc yn))) (* r r))))

(defn mem [cell node]
  (let [d (dist cell node)
        c -0.1]
    (if (<= d 0) 1 (exp (* c d)))))

(defn best-of [cell n1 n2]
  (cond
    (nil? n1) n2
    (nil? n2) n1
    :else (let [m1 (mem cell n1)
                m2 (mem cell n2)]
            (cond
              (< m1 m2) n2
              (< m2 m1) n1
              :else n1))))

(defn nearest-node [cell nodes]
  (let [n* (loop [best-node nil 
                 nodes nodes]
            (if (empty? nodes)
              best-node
              (recur (best-of cell best-node (first nodes)) (rest nodes))))]
    [n* (mem cell n*)]))

(defn cell-opacity [c]
  (if (< (.random js/Math) 0.05)
    1.0
    0.3))

(defn update-cells [{:keys [cells nodes links]}]
  (doseq [c cells]
    (let [[n* m] (nearest-node c nodes)]
      (aset c "opacity" (cell-opacity c))
      (aset c "color" (if (zero? m) "transparent" (.-color n*))))))

(defn update-d3 [{:keys [g cells nodes links hexbin]}]
  (-> g
      (.selectAll "path")
      (.data cells)
      (.attr "fill" #(.-color %1))
      (.attr "opacity" #(.-opacity %1))
      (.enter)
      (.append "path")
      (.attr "d" #(.hexagon hexbin (.-radius %1)))
      (.attr "transform" #(str "translate(" (.-x %1) "," (.-y %1) ")"))
      (.attr "opacity" #(.-opacity %1))
      (.attr "fill" (fn [d] (.-color d)))))

(defn update-loop [{:keys [g cells nodes links hexbin i] :or {i 0} :as args}]
  (update-cells args)
  (update-d3 args)
  ;(draw/raw-repaint nodes links)
  ;(when (< i 10)
  ;  (js/setTimeout #(update-loop (assoc args :i (inc i))) 1000))
)

(defn scatter [nodes links]
  (aset js/window "nodes" nodes)
  (colorize-nodes nodes)
  (let [[w h] (js->clj (d/get-size))
        hexbin (-> js/d3 
                   (.hexbin) 
                   (.size (d/get-size))
                   (.radius hex-radius)
                   (.x #(aget % "x"))
                   (.y #(aget % "y")))
        cells (get-cells hexbin)
        g (-> js/d3
                (.select "#svg-div")
                (.append "svg")
                (.attr "width" w)
                (.attr "height" h)
                (.append "g"))]
    (aset js/window "cells" cells)
    (update-loop {:g g
                  :cells cells 
                  :nodes nodes 
                  :links links 
                  :hexbin hexbin})))
