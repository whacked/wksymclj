(ns wksymclj.example.mxgraph-renderer
  (:require [goog.dom :as gdom]
            [wksymclj.nodejs-interface.fileio :as fio]
            [wksymclj.ui.browser-interop
             :refer [clear-dom-element!]]
            [wksymclj.data-manipulation.xml
             :refer [xml->js js->xml]]
            [wksymclj.ui.mxgraph :as mx]))

(defn render-hello-world!
  ;; hello world example
  ;; see https://jgraph.github.io/mxgraph/javascript/examples/helloworld.html
  ;; (render-hello-world! (gdom/getElement "panel-A"))
  [target-el]
  (let [mx-container (clear-dom-element! target-el)
        graph (new mx/mxGraph mx-container)
        parent (.getDefaultParent graph)]
    (new mx/mxRubberband graph)
    (let [v1 (.insertVertex graph
                            parent
                            nil "Hello" 20 20 80 30)
          v2 (.insertVertex graph
                            parent
                            nil "World" 200 150 80 30)
          e1 (.insertEdge graph
                          parent
                          nil "some edge" v1 v2)]
      (.. graph getModel endUpdate)

      (let [node (-> (new mx/mxCodec)
                     (js-invoke "encode" (.getModel graph)))]
        (-> (js-invoke mx/mxUtils "getXml" node)
            (xml->js)
            (js->xml))))))

(comment
  (render-mxgraph-data-to-element!
   {:mxGraphModel
    {:root
     {:mxCell [{:_id 0}
               {:_id 1 :_parent 0}
               {:_id 2 :_parent 1 :mxGeometry
                {:_x 380 :_width 140 :_height 30 :_as "geometry"} :_value "Interval 1" :_vertex 1}
               {:_id 3 :_parent 1 :mxGeometry
                {:_x 200 :_y 80 :_width 380 :_height 30 :_as "geometry"} :_value "Interval 2" :_vertex 1}
               {:_id 4 :_parent 1 :mxGeometry
                {:_x 40 :_y 140 :_width 260 :_height 30 :_as "geometry"} :_value "Interval 3" :_vertex 1}
               {:_id 5 :_parent 1 :mxGeometry
                {:_x 120 :_y 200 :_width 240 :_height 30 :_as "geometry"} :_value "Interval 4" :_vertex 1}
               {:_id 6 :_parent 1 :mxGeometry
                {:_x 420 :_y 260 :_width 80 :_height 30 :_as "geometry"} :_value "Interval 5" :_vertex 1}
               {:_id 7 :_parent 1 :mxGeometry
                {:Array {:Object {:_x 420 :_y 60} :_as "points"} :_as "geometry"} :_value "Transfer1" :_source 2 :_target 3 :_edge 1}
               {:_id 8 :_parent 1 :mxGeometry
                {:Array {:Object {:_x 600 :_y 60} :_as "points"} :_y -30 :_relative 1 :_as "geometry"} :_value nil :_source 2 :_target 6 :_edge 1}
               {:_id 9 :_parent 1 :mxGeometry
                {:Array {:Object {:_x 260 :_y 120} :_as "points"} :_as "geometry"} :_value "Transfer3" :_source 3 :_target 4 :_edge 1}
               {:_id 10 :_parent 1 :mxGeometry
                {:Array {:Object {:_x 200 :_y 180} :_as "points"} :_as "geometry"} :_value "Transfer4" :_source 4 :_target 5 :_edge 1}
               {:_id 11 :_parent 1 :mxGeometry
                {:Array {:Object {:_x 460 :_y 155} :_as "points"} :_y -10 :_relative 1 :_as "geometry"} :_value "Transfer5" :_source 4 :_target 6 :_edge 1}]}}}
   (gdom/getElement "panel-A")))
