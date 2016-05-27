(ns imdb.draw)

;; The driver for d3 visualization

;; layout configuration
(def state (atom {:dimension      #js [600 600] 
                  :alpha          0.1
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
(defn- mx [node]
  (let [x (.-x node)
        r (.-size node)]
    (- x r)))
(defn- my [node]
  (let [y (.-y node)
        r (.-size node)]
    (- y r)))
(defn- Mx [node]
  (let [x (.-x node)
        r (.-size node)]
    (+ x r)))
(defn- My [node]
  (let [y (.-y node)
        r (.-size node)]
    (+ y r)))

(defn- bounds
  [nodes]
  (let [w (width)
        h (height)]
    (loop [nodes nodes
           minx  (mx (first nodes))
           miny  (my (first nodes))
           maxx  (Mx (first nodes))
           maxy  (My (first nodes))]
      (if (empty? nodes)
        [minx miny maxx maxy]
        (recur (rest nodes)
               (min minx (mx (first nodes)))
               (min miny (my (first nodes)))
               (max maxx (Mx (first nodes)))
               (max maxx (My (first nodes))))))))

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
      (aset "fillStyle" "#555")
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
      (aset "strokeStyle" "#eee")
      (aset "lineWidth" 5)
      (.beginPath)
      (.moveTo x1 y1)
      (.lineTo x2 y2)
      (.stroke)
      (.closePath)
      (.restore))))

(defn do-draw [nodes f]
  (let [[sx sy tx ty] (get-fitting nodes)]
    (doto canvas
      (.save)
      (.scale sx sy)
      (.translate tx ty))
    (f)
    (.restore canvas)))

;; repaint the entire canvas of all the nodes
;; and links.
;; It also rescales the canvas if necessary to
;; make sure that all the nodes fit in the
;; canvas.
(defn repaint
  [nodes links]
  (clear)
  (do-draw nodes #(do (doseq [link links] (draw-link link))
                      (doseq [node nodes] (draw-node node)))))

(defn d3-tick [{:keys [nodes links]}]
  (resolve! nodes)
  (repaint nodes links))

; ========================================================

(def M 1000000)

(defn total-rawsize [nodes]
  (reduce + 0 (map #(.-rawsize %) nodes)))

(defn random [r]
  (.randomGaussian js/Math 0 (/ r 2)))

(defn rand-255 []
  (int (* 255 (.random js/Math))))

(defn rand-rgb []
  (str "RGB(" (rand-255) "," (rand-255) "," (rand-255)))

(defn draw-dot [x y]
  (doto canvas
    (.beginPath)
    (.arc x y 3 0 (* 2 PI))
    (.fill)
    (.closePath)))

(defn- -populate [nodes]
  (println "-pouplate")
  (let [N (total-rawsize nodes)]
    (doseq [node nodes]
      (let [r (.-size node)
            x (.-x node)
            y (.-y node)
            n (int (max 4 (* M (/ (.-rawsize node) N))))]
        (do (.save canvas)
            (aset canvas "fillStyle" (rand-rgb))
            (aset canvas "globalAlpha" 0.05)
            (dotimes [i n]
              (let [dx (random r) dy (random r)]
                (draw-dot (+ x dx) (+ y dy))))
            (.restore canvas))))))

(defn populate [nodes]
  (do-draw nodes #(-populate nodes)))
