(ns imdb.scatter
  (:require [imdb.data :as d]))

(def blue "#33C4FF")

(defn- get-cells [hexbin]
  (let [centers (-> hexbin (.centers) (js->clj))]
    (clj->js (into [] (for [[x y] centers]
                        {:x x
                         :y y
                         :radius 9
                         :color blue})))))

(defn scatter [nodes link]
  (let [[w h] (js->clj (d/get-size))
        hexbin (-> js/d3 
                   (.hexbin) 
                   (.size (d/get-size))
                   (.radius 10)
                   (.x #(aget % "x"))
                   (.y #(aget % "y")))
        cells (get-cells hexbin)
        svg (-> js/d3
                (.select "#svg-div")
                (.append "svg")
                (.attr "width" w)
                (.attr "height" h)
                (.append "g"))]
    (-> svg
        (.selectAll "path")
        (.data cells)
        (.enter)
        (.append "path")
        (.attr "d" #(.hexagon hexbin (.-radius %1)))
        (.attr "transform" #(str "translate(" (.-x %1) "," (.-y %1) ")"))
        (.attr "fill" #(.-color %1)))))

