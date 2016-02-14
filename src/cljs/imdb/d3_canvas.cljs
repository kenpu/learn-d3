(ns imdb.d3-canvas
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [imdb.graph :as graph]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

;; layout configuration
(def -conf (atom {:dimension      #js [800 500] 
                  :gravity        0.1
                  :linkStrength   0.2
                  :charge         -300}))

(defn cnf [key]
  (@-conf key))

(defn width []
  (-> @-conf :dimension (aget 0)))

(defn height []
  (-> @-conf :dimension (aget 1)))


(def PI (.-PI js/Math))
(def sqrt #(.sqrt js/Math %))



;; dealing with all of d3 and canvas needs
;;

(defn mk-nodes [edges]
  (clj->js 
    (into [] 
          (map #(hash-map :label % :size 3) (graph/get-vertices edges)))))

(defn lookup-index
  [nodes]
  (let [node-indexed (map-indexed vector nodes)]
    (apply hash-map (mapcat (fn [[i node]]
                               [(.-label node) i]) node-indexed))))

;; makes the links
;; Links is an array of {:source index :target index}
(defn mk-links 
  [nodes edges]
  (let [lookup (lookup-index nodes)]
    (clj->js
      (into [] (for [[u v] edges]
                 {:source (lookup u)
                  :target (lookup v)})))))

;; computes the scaling factor
;; that is suitable for drawing
;;
(defn get-fitting
  [nodes]
  [0.1 0.1 100 100])




;; creates a force layout
;; It uses the d3.layout
(defn force-layout
  [edges tree-edges tick]
  (let [nodes  (mk-nodes edges)
        links  (mk-links nodes tree-edges)
        callback (fn [e] (tick e nodes links))]
    (.. d3.layout 
        (force)
        (gravity (cnf :gravity))
        (charge (fn [d i] (cnf :charge)))
        (links links)
        (linkStrength (cnf :linkStrength))
        (nodes nodes)
        (size (cnf :dimension))
        (on "tick" callback))))

(defn bounding-box [node]
  (let [r (+ (.-size node) 16)]
    [(- (.-x node) r)
     (- (.-y node) r)
     (+ (.-x node) r)
     (+ (.-y node) r)]))

(defn resolve-collision! 
  [p node]
  (let [dx (- (.-x node) (.-x p))
        dy (- (.-y node) (.-y p))
        l (sqrt (+ (* dx dx) (* dy dy)))
        r (+ (.-size node) (.-size p))]
    (if (< l r)
      (let [l (* 0.5 (/ (- l r) l))]
        (set! (.-x node) (- (.-x node) (* dx l)))
        (set! (.-y node) (- (.-y node) (* dy l)))
        (set! (.-x p)    (+ (.-x p) (* dx l)))
        (set! (.-y p)    (+ (.-y p) (* dy l)))))))

;; makes a collision resolving function
;; The resulting function is to be used
;; as a visitor called by the quadtree
;; scan
(defn mk-collision
  [node]
  (let [[nx1 ny1 nx2 ny2] (bounding-box node)]
    (fn [quad x1 y1 x2 y2]
      (let [p (.-point quad)]
        (if (and (not (nil? p))
                 (not (= p node)))
          (do (resolve-collision! p node)
              (or (> x1 nx1) (< x2 nx1) (> y1 ny2) (< y2 ny1))))))))

;; resolves all the collisions among the nodes
;; For performance, we use a quadtree spatial index
;; as provided by d3.geom.quadtree
(defn resolve!
  [nodes]
  (let [q (.quadtree js/d3.geom nodes)]
    (doseq [node nodes]
      (.visit q (mk-collision node)))))


;; get the canvas
(def canvas
  (-> js/document
      (.querySelector "canvas")
      (.getContext "2d")))

(defn clear []
  (let [c (.-canvas canvas)
        w (.-width c)
        h (.-height c)]
  (.clearRect canvas 0 0 w h)))


(defn draw-node
  [node]
  (let [x (.-x node)
        y (.-y node)
        r (.-size node)]
    (doto canvas
      (.save)
      (aset "fillStyle" "#A9E2F3")
      (aset "globalAlpha" 0.6)
      (.beginPath)
      (.arc x y r 0 (* 2 PI))
      (.fill)
      (.closePath)
      (.restore))))

(defn draw-link
  [link]
  (let [n1 (.-source link)
        n2 (.-target link)
        x1 (.-x n1)
        y1 (.-y n1)
        x2 (.-x n2)
        y2 (.-y n2)]
    (doto canvas
      (.save)
      (aset "strokeStyle" "#888")
      (aset "lineWidth" 5)
      (.beginPath)
      (.moveTo x1 y1)
      (.lineTo x2 y2)
      (.stroke)
      (.closePath)
      (.restore))))

(defn repaint
  [nodes links]
  (let [[sx sy tx ty] (get-fitting nodes)]
    (clear)
    (doto canvas
      (.save)
      (.translate tx ty)
      (.scale sx sy))
    (doseq [link links]
      (draw-link link))
    (doseq [node nodes]
      (draw-node node))
    (doto canvas
      (.restore))))





(defn tick
  [e nodes links]
  (resolve! nodes)
  (repaint nodes links))
