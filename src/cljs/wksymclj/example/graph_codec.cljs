(ns wksymclj.example.graph-codec
  (:require [goog.dom :as gdom]
            [wksymclj.data-manipulation.graph :as grf]
            [wksymclj.example.dagre-state-view]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.ui.mxgraph :as mx]
            [wksymclj.codec.graph :as grf-codec]))

(comment
  ;; example
  (let [flow-graph (grf/state-declaration-to-flow-graph
                    wksymclj.example.dagre-state-view/state-declaration-list)]
    (-> (dagre/make-dagre
         (:node-list flow-graph)
         (:edge-list flow-graph))
        (grf-codec/dagre-graph-to-mxgraph-data)
        (mx/render-mxgraph-data-to-element!
         (gdom/getElement "panel-A")))))
