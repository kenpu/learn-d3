(ns imdb.d3-canvas
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljsjs.d3]
            [imdb.graph :as graph]
            [imdb.pipeline :refer [pipeline]]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

;; The driver for d3 visualization

;; layout configuration
(def state (atom {:dimension      #js [800 500] 
                  :gravity        0.1
                  :linkStrength   0.2
                  :charge         -300}))

;; get the configuration of a paritcular key
(defn cnf [key]
  (@state key))

;; get the width of the configuration
(defn width []
  (-> @state :dimension (aget 0)))

;; get the height of the configuration
(defn height []
  (-> @state :dimension (aget 1)))


(def PI (.-PI js/Math))
(def sqrt #(.sqrt js/Math %))

;; computes the scaling factor
;; that is suitable for drawing
;; We scan through the points and get the min,max
;; coordinates, and computes the scaling
;; required to fit all the data points in
;; the configured width and height.
(defn- bounds
  [nodes]
  (let [w (width)
        h (height)]
    (loop [nodes nodes
           minx  (.-x (first nodes))
           miny  (.-y (first nodes))
           maxx  minx
           maxy  miny]
      (if (empty? nodes)
        [minx miny maxx maxy]
        (recur (rest nodes)
               (min minx (.-x (first nodes)))
               (min miny (.-y (first nodes)))
               (max maxx (.-x (first nodes)))
               (max maxx (.-y (first nodes))))))))

;; computes the [scalex scaley translatex translatey]
;; to fit all the nodes in the configured width/height
;; starting at (0,0)
(defn- get-fitting
  [nodes]
  (let [[minx miny maxx maxy] (bounds nodes)
        w (width)
        h (height)
        scalex (/ w (- maxx minx))
        scaley (/ h (- maxy miny))
        scale  (min scalex scaley)]
    [scale scale (- minx) (- miny)]))

;; get the bounding box of a node centered at its
;; x,y with a square radius of 16.
;; This is used to query collision nodes during
;; collision resolution.
(defn bounding-box [node]
  (let [r (+ (.-size node) 16)]
    [(- (.-x node) r)
     (- (.-y node) r)
     (+ (.-x node) r)
     (+ (.-y node) r)]))

;; resolves the collision between nodes `p` and `node`.
;; It uses the traditional point displacmeent method.
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
(defn mk-collision-fn
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
      (.visit q (mk-collision-fn node)))))


;; get the canvas drawing context
(def canvas
  (-> js/document
      (.querySelector "canvas")
      (.getContext "2d")))

;; clear the canvas
(defn clear []
  (let [c (.-canvas canvas)
        w (.-width c)
        h (.-height c)]
  (.clearRect canvas 0 0 w h)))


;; draw a node
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

;; draw a link
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

;; repaint the entire canvas of all the nodes
;; and links.
;; It also rescales the canvas if necessary to
;; make sure that all the nodes fit in the
;; canvas.
(defn repaint
  [nodes links]
  (let [[sx sy tx ty] (get-fitting nodes)]
    (clear)
    (doto canvas
      (.save)
      (.scale sx sy)
      (.translate tx ty)
      (.restore))
    (doseq [link links]
      (draw-link link))
    (doseq [node nodes]
      (draw-node node))
    (doto canvas
      (.restore))))

(defn d3-tick
  [nodes links]
  (resolve! nodes)
  (repaint nodes links))




;; creates a force layout
;; It uses the d3.layout
(defn force-layout
  [nodes links]
  (let [[c1 c2] (pipeline)
        layout (.force d3.layout)]
    (put! c2 "Init")
    (go-loop [i 0]
             (let [{:keys [message init tick]} (<! c1)
                   callback (fn [e]
                              (do (d3-tick nodes links)
                                  (when-not (nil? tick)
                                    (tick nodes links))))
                   pingback (fn [e]
                              (do
                                (put! c2 "Ping")))]
               (if (nil? message)
                 (println "[CANVAS] Nil annealing in the pipeline")
                 (do
                   (println "[CANVAS] Annealing stage" message)
                   (when-not (nil? init)
                     (init :layout layout 
                           :state state
                           :nodes nodes
                           :links links
                           :counter i))

                   (js/console.debug "kens_debug" nodes links)

                   ;; configure the layout
                   ;; and start the simulation
                   (.. layout
                       (gravity (cnf :gravity))
                       (charge  (cnf :charge))
                       (links   links)
                       (linkStrength (cnf :linkStrength))
                       (nodes nodes)
                       (size (cnf :dimension))
                       (on "tick" callback)
                       (on "end" pingback)
                       (start)
                  )

                  (recur (inc i)))))
    )))


