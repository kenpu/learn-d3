(ns imdb.scatter
  (:require [imdb.data :as d]))

(def blue "#33C4FF")
(def red "#f00")

(defn- get-cells [hexbin]
  (let [centers (-> hexbin (.centers) (js->clj))]
    (clj->js (into [] (for [[x y] centers]
                        {:x x
                         :y y
                         :radius 9
                         :color blue})))))

(defn- assoc-cells 
  "assocate the cells to the nearest
  nodes"
  [cells nodes links])

(defn rand-color []
  (if (< (.random js/Math) 0.1) red blue))

(defn update-cells [cells nodes links]
  (doseq [c cells]
    (aset c "color" (rand-color))))

(defn update-d3 [g cells nodes links tools]
  (-> g
      (.selectAll "path")
      (.data cells)
      (.attr "fill" #(.-color %1))
      (.enter)
      (.append "path")
      (.attr "d" #(.hexagon (:hexbin tools) (.-radius %1)))
      (.attr "transform" #(str "translate(" (.-x %1) "," (.-y %1) ")"))
      (.attr "fill" (fn [d] (.-color d)))))

(defn update-loop [g cells nodes links tools]
  (js/setTimeout (fn []
                   (update-cells cells nodes links)
                   (update-d3 g cells nodes links tools)
                   (update-loop g cells nodes links tools)) 300))

(defn scatter [nodes links]
  (let [[w h] (js->clj (d/get-size))
        hexbin (-> js/d3 
                   (.hexbin) 
                   (.size (d/get-size))
                   (.radius 10)
                   (.x #(aget % "x"))
                   (.y #(aget % "y")))
        cells (get-cells hexbin)
        g (-> js/d3
                (.select "#svg-div")
                (.append "svg")
                (.attr "width" w)
                (.attr "height" h)
                (.append "g"))]
    (update-loop g cells nodes links {:hexbin hexbin})))
