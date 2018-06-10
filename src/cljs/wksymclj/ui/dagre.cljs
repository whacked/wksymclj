(ns wksymclj.ui.dagre
  (:require [cljs.nodejs :as nodejs]))

(def dagre (nodejs/require "dagre"))
(def dagre-layout (aget dagre "layout"))
(def DagreGraph (aget dagre "graphlib" "Graph"))

;; for dagre/panzoom demo
(def $svg-header {:version "1.1"
                  :baseProfile "full"
                  ;; :xmlns "http://www.w3.org/2000/svg"
                  :style {:width "600px"
                          :height "800px"
                          :border "2px dashed red"
                          :position "absolute"
                          }})
(def $svg-defs [:defs
                [:marker {
                          :id "markerArrow" 
                          :markerWidth "4"
                          :markerHeight "4"
                          :viewBox "-6 -6 12 12" 
                          :refX "-2"
                          :refY "0" 
                          :markerUnits "strokeWidth" 
                          :orient "auto"
                          }
                 [:polygon {:points "-2,0 -5,5 5,0 -5,-5"
                            :style {:fill "#000000"
                                    :stroke "#600"
                                    :strokeWidth "1px"}}]
                 
                 [:circle {:cx 0
                           :cy 0
                           :r 4
                           :style {:fill "none"
                                   :stroke "black"
                                   :strokeWidth 0.5}}]
                 ]])


(defn make-dagre
  ;; note https://github.com/cpettitt/dagre/issues/161
  ;; in usage of dagre graph attributes
  [node-list edge-list
   & [layout-config]]
  (let [graph (new DagreGraph (clj->js {:compound true}))]
    (doto graph
      (.setGraph (clj->js (or layout-config {})))
      (.setDefaultEdgeLabel (fn [] (clj->js {}))))
    
    (let [expand-node-name (fn [name cond]
                             (str name "--" cond))]
      ;; process nodes
      (doseq [node-struct node-list]
        (.setNode graph (:name node-struct)
                  (clj->js node-struct)))
      
      ;; process edges
      (doseq [[pr-name po-name edge-data] edge-list]
        (if edge-data
          (.setEdge graph pr-name po-name
                    (clj->js edge-data))
          (.setEdge graph pr-name po-name)))
      (dagre-layout graph))
    graph))

(defn get-dagre-node-seq [dagre-graph]
  (vec (->> (.nodes dagre-graph)
            (map #(js->clj (.node dagre-graph %)
                           :keywordize-keys true)))))

(defn get-dagre-edge-seq [dagre-graph]
  (vec (->> (.edges dagre-graph)
            (map #(merge
                   (js->clj % :keywordize-keys true)
                   (js->clj (.edge dagre-graph %)
                            :keywordize-keys true))))))

;; #+pure
(defn get-node-id-mapping
  ([node-seq] (get-node-id-mapping 0))
  ([node-seq start-id]
   (loop [remain-node node-seq
          node-id-mapping {}]
     (if (empty? remain-node)
       node-id-mapping
       (let [node (first remain-node)
             node-name (:name node)
             node-id (node-id-mapping node-name
                                      (+ start-id
                                         (count node-id-mapping)))]
         (recur (rest remain-node)
                (assoc node-id-mapping
                       node-name node-id)))))))
