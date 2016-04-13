(comment 
  " A pipeline accepts all stateful data, and keeps returning
  another pipeline.
  Each pipeline has its own event handlers for :start, :end, and :tick.
  ")
(defn force-layout
  [data-set
   hints
   pipeline]
  (let [layout (.force d3.layout)
        nodes (mk-nodes data-set hints)
        links (mk-links data-set hints)
        state (atom nil)
        [c1 c2] (pipeline state layout nodes links)]
    (go-loop []
          (let [f (<! c1)]
            (if (nil? f)
              nil
              (do
                  (f :conf layout nodes links state)
                  (.. layout
                      (.on "tick" (f :tick))
                      (.on "end" #(go (>! c2 %))))
                  (recur c1)))))))

