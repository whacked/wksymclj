(ns wksymclj.ui.graph-codec
  (:require [goog.dom :as gdom]
            [wksymclj.data-manipulation.graph :as grf]
            [wksymclj.example.dagre-state-view]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.ui.mxgraph :as mx]))

(defn dagre-graph-to-mxgraph-data [dagre-graph]
  (let [dagre-nodes (-> dagre-graph
                        (wksymclj.ui.dagre/get-dagre-node-seq))
        dagre-edges (-> dagre-graph
                        (wksymclj.ui.dagre/get-dagre-edge-seq))
        _node-mapping (->> dagre-nodes
                           (map (fn [node]
                                  [(:name node) node]))
                           (into {}))
        _node-id-mapping (wksymclj.ui.dagre/get-node-id-mapping
                          dagre-nodes 2)
        dagre-node-to-mxgraph-vertex
        (fn [dagre-node]
          ;; dagre-node is like
          ;; {:label "Start!"
          ;;  :name "00-home-screen"
          ;;  :x 10 :y 20
          ;;  :width 144 :height 20
          ;;  :type :step}
          ;; mx-node is like
          ;; {:_id 4 :_parent 1 :mxGeometry
          ;;  :_value "Interval 3" :_vertex 1
          ;;  {:_x 40 :_y 140 :_width 260 :_height 30 :_as "geometry"}}
          (let [geom (->> (map dagre-node [:width :height :x :y])
                          (zipmap [:_width :_height :_x :_y])
                          (merge {:_as "geometry"}))
                node-name (:name dagre-node)
                base {:_id (_node-id-mapping node-name)
                      :_parent 1
                      :_vertex 1}]
            (->> (map dagre-node [:label])
                 (zipmap [:_value])
                 (merge base {:mxGeometry geom}))))

        mx-cell-seq (->> dagre-nodes
                         (map dagre-node-to-mxgraph-vertex)
                         ;; WARNING: this is a risky call.
                         ;; From dagre nodes, the revisited nodes
                         ;; will simply show up multiple times.
                         ;; we *probably* need to filter them out.
                         (distinct)
                         (concat [{:_id 0}
                                  {:_id 1 :_parent 0}]))
        mx-edge-start-index (count mx-cell-seq)

        mx-edge-seq
        (loop [remain-edge dagre-edges
               edge-id-mapping {}
               out []]
          (if (empty? remain-edge)
            out
            (let [dagre-edge (first remain-edge)]
              ;; dagre-edge is like
              ;; {:v "00-home-screen"
              ;;  :w "01c-make-a-choice-1"
              ;;  :label "true(END)"
              ;;  :points [{:x 179 :y 20} {:x 179 :y 45} {:x 179 :y 70}]}
              ;; mx-edge is like
              ;; {:_id 7 :_parent 1 :_edge 1
              ;;  :_value "Transfer1"
              ;;  :_source 2 :_target 3
              ;;  :mxGeometry {:_as "geometry"
              ;;               :Array {:_as "points"
              ;;                       :Object {:_x 420 :_y 60}}}}
              (let [pre (:v dagre-edge)
                    post (:w dagre-edge)
                    dagre-node (_node-mapping post)
                    node-width (:width dagre-node)
                    edge-midpt (-> dagre-edge
                                   (:points)
                                   (grf/get-edge-midpt))
                    edge-midx (:x edge-midpt)
                    edge-midy (:y edge-midpt)
                    edge-id (+ mx-edge-start-index
                               (count edge-id-mapping))]
                (recur (rest remain-edge)
                       (assoc edge-id-mapping
                              [pre post] edge-id)
                       (conj out
                             {:_edge 1
                              :_parent 1
                              :_id edge-id
                              :_source (_node-id-mapping pre)
                              :_target (_node-id-mapping post)
                              :_value (:label dagre-edge)
                              :mxGeometry {:_as "geometry"
                                           :_relative 1
                                           :Array {:_as "points"
                                                   :Object {:_x (-> edge-midx
                                                                    (+ (/ node-width 2)))
                                                            :_y edge-midy}}}}))))))]

    {:mxGraphModel
     {:root
      {:mxCell (concat mx-cell-seq mx-edge-seq)}}}))

(comment
  ;; example
  (let [flow-graph (grf/state-declaration-to-flow-graph
                    wksymclj.example.dagre-state-view/state-declaration-list)]
    (-> (wksymclj.ui.dagre/make-dagre
         (:node-list flow-graph)
         (:edge-list flow-graph))
        (dagre-graph-to-mxgraph-data)
        (mx/render-mxgraph-data-to-element!
         (gdom/getElement "panel-A")))))
