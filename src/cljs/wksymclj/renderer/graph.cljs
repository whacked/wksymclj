(ns wksymclj.renderer.graph
  (:require
   [wksymclj.codec.graph :as graph-codec]
   [wksymclj.ui.colorization :as color]
   [wksymclj.ui.mxgraph :as mx
    :refer [underscoreify-keys]]
   [wksymclj.ui.cytoscape :as cyto]
   [wksymclj.codec.cytoscape :as cyto-codec]
   ["cytoscape" :as cytoscape]
   ["cytoscape-dagre" :as cytoscape-dagre]))

(.use cytoscape cytoscape-dagre)

(do
 
  (defprotocol GraphRenderer
    (apply-style-map! [this style-map element-seq])
    (highlight-nodes! [this element-seq])
    (unhighlight-nodes! [this element-seq])
    (highlight-edges! [this element-seq])
    (unhighlight-edges! [this element-seq]))

  (defrecord CytoscapeRenderer
      [graph-object]
      GraphRenderer
      (apply-style-map!
        [this style-map element-seq]
        (doseq [element element-seq]
          (cyto/set-element-style! element style-map)))
      (highlight-nodes!
        [this node-seq]
        (apply-style-map!
         this
         {"background-color" "yellow"
          "border-width" "4px"
          "border-color" "red"}
         node-seq))
      (unhighlight-nodes!
        [this node-seq]
        (apply-style-map!
         this
         {"background-color" ""
          "border-width" ""
          "border-color" ""}
         node-seq))
      (highlight-edges!
        [this edge-seq]
        (apply-style-map!
         this
         {"width" "6"
          "line-color" "yellow"
          "target-arrow-color" "orange"}
         edge-seq))
      (unhighlight-edges!
        [this edge-seq]
        (apply-style-map!
         this
         {"width" "3"
          "line-color" "gray"
          "target-arrow-color" "green"}
         edge-seq)))
 
  (defrecord MxGraphRenderer
      [graph-object]
      GraphRenderer
      (apply-style-map!
        [this style-map element-seq]
        (doseq [element element-seq]
          (mx/set-style graph-object element style-map)))))

(defn set-node-style-map!
  [graph-object node-name style-map]
  (case (graph-codec/get-graph-type graph-object)
    :cytograph
    (cyto/set-element-style!
     (cyto/get-node graph-object node-name)
     style-map)
    
    :mxgraph
    (mx/set-style
     graph-object
     (mx/get-matching-node
      graph-object
      {:name node-name})
     style-map)))

(defn set-edge-style-map!
  [graph-object pre-node-name post-node-name style-map]
  (case (graph-codec/get-graph-type graph-object)
    :cytograph
    (cyto/set-element-style!
     (cyto/get-edge
      graph-object
      pre-node-name post-node-name)
     style-map)
    
    :mxgraph
    (mx/set-style
     graph-object
     (mx/get-matching-edge
      graph-object
      pre-node-name post-node-name)
     style-map)))

(defn highlight-node-in-graph!
  [graph-object node-name]
  (set-node-style-map!
   graph-object
   node-name
   (case (graph-codec/get-graph-type graph-object)
     :cytograph
     {"background-color" "yellow"
      "border-width" "4px"
      "border-color" "red"}

     :mxgraph
     {(aget mx/mxConstants "STYLE_FILLCOLOR") "yellow"
      (aget mx/mxConstants "STYLE_STROKECOLOR") "red"
      (aget mx/mxConstants "STYLE_STROKEWIDTH") 4})))

(defn unhighlight-node-in-graph!
  [graph-object node-name]
  (set-node-style-map!
   graph-object
   node-name
   (case (graph-codec/get-graph-type graph-object)
     :cytograph
     {"background-color" ""
      "border-width" ""
      "border-color" ""}

     :mxgraph
     {})))

(defn highlight-edge-in-graph!
  [graph-object pre-node-name post-node-name]
  (set-edge-style-map!
   graph-object
   pre-node-name post-node-name
   (case (graph-codec/get-graph-type graph-object)
     :cytograph
     {"width" "6"
      "line-color" "yellow"
      "target-arrow-color" "orange"}

     ;; UNVERIFIED
     :mxgraph
     {(aget mx/mxConstants "STYLE_FILLCOLOR") "yellow"
      (aget mx/mxConstants "STYLE_STROKECOLOR") "red"
      (aget mx/mxConstants "STYLE_STROKEWIDTH") 4})))

(defn unhighlight-edge-in-graph!
  [graph-object pre-node-name post-node-name]
  (set-edge-style-map!
   graph-object
   pre-node-name post-node-name
   (case (graph-codec/get-graph-type graph-object)
     :cytograph
     {"width" "3"
      "line-color" "gray"
      "target-arrow-color" "green"}
     
     ;; UNVERIFIED
     :mxgraph
     {})))
