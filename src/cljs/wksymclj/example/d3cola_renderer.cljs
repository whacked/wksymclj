(ns wksymclj.example.d3cola-renderer
  (:require [cljs.nodejs :as nodejs]))

;; this is a direct port of http://ialab.it.monash.edu/webcola/examples/alignment.html

;; need to add to package:
;; d3v4
;; webcola

(def width 800)
(def height 400)


(def cola (nodejs/require "webcola"))
(def d3 (nodejs/require "d3v4"))

(def d3cola
  (-> (.d3adaptor cola d3)
      (.linkDistance 120)
      (.avoidOverlaps true)
      (.size #js [width height])))

(defn get-alignment-bounds [vs c]
  (let [os (aget c "offsets")]
    (if (= (aget c "axis") "x")
      (let [x (aget
               vs
               (-> (aget os 0)
                   (aget "node"))
               "x")]
        (aset c "bounds"
              (new (aget cola "Rectangle")
                   x x
                   (.apply Math/min js/Math
                           (.map os (fn [o] (- (aget vs
                                                     (aget o "node")
                                                     "bounds"
                                                     "y")
                                               20))))
                   (.apply Math/max js/Math
                           (.map os (fn [o] (+ (aget vs
                                                     (aget o "node")
                                                     "bounds"
                                                     "Y")
                                               20)))))))
      (let [y (aget
               vs
               (-> (aget os 0)
                   (aget "node"))
               "y")]
        (aset c "bounds"
              (new (aget cola "Rectangle")
                   (.apply Math/min js/Math
                           (.map os (fn [o] (- (aget vs
                                                     (aget o "node")
                                                     "bounds"
                                                     "x")
                                               20))))
                   (.apply Math/max js/Math
                           (.map os (fn [o] (+ (aget vs
                                                     (aget o "node")
                                                     "bounds"
                                                     "X")
                                               20))))
                   y y))))
    (aget c "bounds")))

(comment
  (do
    (-> (.select d3 "#panel-B")
        (.select "svg")
        (.remove))

    (let [svg (-> (.select d3 "#panel-B")
                  (.append "svg")
                  (.attr "width" width)
                  (.attr "height" height))
          graph (clj->js {:nodes [{"name" "a" "width" 60 "height" 40} 
                                  {"name" "b" "width" 60 "height" 40} 
                                  {"name" "c" "width" 60 "height" 40} 
                                  {"name" "d" "width" 60 "height" 40} 
                                  {"name" "e" "width" 60 "height" 40}] 
                          :links [{"source" 1 "target" 2} 
                                  {"source" 2 "target" 0} 
                                  {"source" 2 "target" 3} 
                                  {"source" 2 "target" 4}] 
                          :constraints [{"type" "alignment" 
                                         "axis" "x" 
                                         "offsets" [{"node" "1" "offset" "0"} 
                                                    {"node" "2" "offset" "0"} 
                                                    {"node" "3" "offset" "0"}]} 
                                        {"type" "alignment" 
                                         "axis" "y" 
                                         "offsets" [{"node" "0" "offset" "0"} 
                                                    {"node" "1" "offset" "0"} 
                                                    {"node" "4" "offset" "0"}]}]})
          ]
      (-> (aget graph "nodes")
          (.forEach (fn [v]
                      (aset v "x" 400)
                      (aset v "y" 50))))
      (-> d3cola
          (.nodes (aget graph "nodes"))
          (.links (aget graph "links"))
          (.constraints (aget graph "constraints"))
          (.start 10 10 10))

      (let [link (-> (.selectAll svg ".link")
                     (.data (aget graph "links"))
                     (.enter)
                     (.append "line")
                     (.attr "class" "link")
                     (.attr "style" "stroke:#999;stroke-width:3px;"))
            guideline (-> (.selectAll svg ".guideline")
                          (.data (-> (aget graph "constraints")
                                     (.filter (fn [c]
                                                (= (aget c "type") "alignment")))))
                          (.enter)
                          (.append "line")
                          (.attr "style" "stroke:red;stroke-width:4px;")
                          (.attr "class" "guideline")
                          (.attr "stroke-dasharray" "5,5"))
            color (.scaleOrdinal d3 (aget d3 "schemeCategory20"))
            node (-> (.selectAll svg ".node")
                     (.data (aget graph "nodes"))
                     (.enter)
                     (.append "rect")
                     (.attr "class" "node")
                     (.attr "style" "stroke:#fff;stroke-width:1.5px;cursor:move;")
                     (.attr "width" (fn [d] (aget d "width")))
                     (.attr "height" (fn [d] (aget d "height")))
                     (.attr "rx" 5)
                     (.attr "ry" 5)
                     (.style "fill" (fn [d] (color 1)))
                     (.call (aget d3cola "drag")))
            label (-> (.selectAll svg ".label")
                      (.data (aget graph "nodes"))
                      (.enter)
                      (.append "text")
                      (.attr "class" "label")
                      (.attr "style" "fill: white;font-family:Sans Serif;font-size: 25px;text-anchor: middle;cursor: move;")
                      (.text (fn [d] (aget d "name")))
                      (.call (aget d3cola "drag")))
            
            ]
        (-> node
            (.append "title")
            (.text (fn [d] (aget d "name"))))

        (.on d3cola "tick"
             (fn []
               (-> link
                   (.attr "x1" (fn [d] (aget d "source" "x")))
                   (.attr "y1" (fn [d] (aget d "source" "y")))
                   (.attr "x2" (fn [d] (aget d "target" "x")))
                   (.attr "y2" (fn [d] (aget d "target" "y"))))
               (-> guideline
                   (.attr "x1" (fn [d] (-> (get-alignment-bounds
                                            (aget graph "nodes")
                                            d)
                                           (aget "x"))))
                   (.attr "y1" (fn [d] (aget d "bounds" "y")))
                   (.attr "x2" (fn [d] (aget d "bounds" "X")))
                   (.attr "y2" (fn [d] (aget d "bounds" "Y"))))
               (-> node
                   (.attr "x" (fn [d] (- (aget d "x")
                                         (/ (aget d "width") 2))))
                   (.attr "y" (fn [d] (- (aget d "y")
                                         (/ (aget d "height") 2)))))
               (-> label
                   (.attr "x" (fn [d] (aget d "x")))
                   (.attr "y" (fn [d]
                                (this-as this
                                  (let [bbox (.getBBox this)
                                        h (aget bbox "height")]
                                    (+ (aget d "y")
                                       (/ h 4)))))))))))))
