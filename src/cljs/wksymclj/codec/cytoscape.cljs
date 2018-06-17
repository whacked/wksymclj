(ns wksymclj.codec.cytoscape
  (:require [goog.dom :as gdom]
            [cljs.nodejs :as nodejs]
            [wksymclj.data-manipulation.graph :as grf]
            [cljs.spec.alpha :as spec]))

(def cytoscape (nodejs/require "cytoscape"))

(defn flowgraph-to-cytoscape-node [flowgraph-node]
  {:pre [(spec/valid? ::grf/FlowgraphNode flowgraph-node)
         (or (:name flowgraph-node)
             (:id flowgraph-node))]}
  (let [xy (select-keys flowgraph-node [:x :y])
        pos (if-not (empty? xy)
              {:position xy})]
    (merge
     {:data
      (-> (if (:id flowgraph-node)
            flowgraph-node
            (assoc flowgraph-node :id (:name flowgraph-node)))
          (dissoc :x :y))}
     pos)))

(defn flowgraph-to-cytoscape-edge [flowgraph-edge]
  {:pre [(spec/valid? ::grf/FlowgraphEdge flowgraph-edge)]}
  (let [src (nth flowgraph-edge 0)
        tgt (nth flowgraph-edge 1)]
   {:data {:id (str "edge:" src "--" tgt)
           :source src
           :target tgt}}))

(defn make-cytoscape-elements [node-list edge-list]
  (vec
   (concat (->> node-list (map to-cytoscape-node))
           (->> edge-list (map to-cytoscape-edge)))))

(defn cytoscape-graph-to-json [cyto-graph]
  (.json cyto-graph))

(defn cytoscape-graph-to-data [cyto-graph]
  (-> cyto-graph
      (cytoscape-graph-to-json)
      (js->clj :keywordize-keys true)))
