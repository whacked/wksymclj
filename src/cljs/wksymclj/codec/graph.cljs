(ns wksymclj.codec.graph
  (:require [goog.dom :as gdom]
            [wksymclj.data-manipulation.graph :as grf]
            [wksymclj.example.dagre-state-view]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.ui.mxgraph :as mx
             :refer [underscoreify-keys]]))

(comment
  ;; alternative method of building mx-cell-seq.
  ;; if you use this, make sure you update the
  ;; mxGeometry section to use the new :_as src/tgt
  ;; marker
  (loop [node-remain (dagre/get-dagre-node-seq flow-dagre)
         edge-remain (dagre/get-dagre-edge-seq flow-dagre)
         mx-out [{:_id 0}
                 {:_id 1 :_parent 0}]
         id-mapper (->> mx-out
                        (map (fn [m] [(:_id m) (:_id m)]))
                        (into {}))]
    
    (cond (and (empty? node-remain)
               (empty? edge-remain))
          mx-out

          (seq node-remain)
          (let [dagre-node (first node-remain)
                node-name (:name dagre-node)
                node-id (id-mapper node-name (count id-mapper))
                mx-node {:_id node-id
                         :_parent 1 :_vertex 1
                         :_value (:label dagre-node) ;; or :name ?
                         :mxGeometry (-> (dagre->mx-geometry dagre-node)
                                         (conj [:_as "geometry"]))}]
            (recur (rest node-remain)
                   edge-remain
                   (conj mx-out mx-node)
                   (conj id-mapper
                         [node-name node-id])))

          (seq edge-remain)
          (let [dagre-edge (first edge-remain)
                pre-name (dagre-edge :v)
                post-name (dagre-edge :w)
                edge-id (count id-mapper)
                mx-edge {:_id edge-id
                         :_parent 1 :_edge 1
                         :_value (:label dagre-edge) ;; or :name ?
                         :_source (id-mapper pre-name)
                         :_target (id-mapper post-name)
                         :mxGeometry {;; fix this
                                      }}]
            (recur node-remain
                   (rest edge-remain)
                   (conj mx-out mx-edge)
                   (conj id-mapper
                         [[pre-name post-name] edge-id]))))))

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
          (if-not dagre-node
            {}
            (let [geom (->> (map dagre-node [:width :height :x :y])
                            (zipmap [:_width :_height :_x :_y])
                            (merge {:_as "geometry"}))
                  node-name (:name dagre-node)
                  base {:_id (_node-id-mapping node-name)
                        :_parent 1
                        :_vertex 1}]
              (->> (map dagre-node [:label])
                   (zipmap [:_value])
                   (merge base {:mxGeometry geom})))))

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
              ;;  :mxGeometry {:mxPoint
              ;;               [{:_x 370 :_y 660
              ;;                 :_as "sourcePoint"}
              ;;                {:_x 420 :_y 610
              ;;                 :_as "targetPoint"}]
              ;;               :Array {:mxPoint {:_x 490 :_y 410}
              ;;                       :_as "points"}
              ;;               :_relative 1
              ;;               :_as "geometry"}}
              (let [pre (:v dagre-edge)
                    post (:w dagre-edge)
                    dagre-node (_node-mapping post)
                    node-width (:width dagre-node)

                    dagre-points (:points dagre-edge)
                    edge-midpt (-> dagre-points
                                   (grf/get-edge-midpt))
                    edge-midx (:x edge-midpt)
                    edge-midy (:y edge-midpt) ;; alternative to pt2y
                    edge-pt2y (-> dagre-points
                                  (second)
                                  (:y))
                    edge-id (+ mx-edge-start-index
                               (count edge-id-mapping))]
                (recur (rest remain-edge)
                       (assoc edge-id-mapping
                              [pre post] edge-id)
                       (conj out
                             (merge
                              (if-let [label (:label dagre-edge)]
                                {:_value label})
                              {:_edge 1
                               :_parent 1
                               :_id edge-id
                               :_source (_node-id-mapping pre)
                               :_target (_node-id-mapping post)
                               :mxGeometry (merge
                                            ;; WARNING: this assumes that dagre points
                                            ;; <= 2 mean we don't need waypoints. this
                                            ;; is not a tested assumption.
                                            (if (< 2 (count dagre-points))
                                              {:Array {:_as "points"
                                                       :mxPoint {:_x (-> edge-midx
                                                                         (+ (/ node-width 2)))
                                                                 :_y edge-pt2y}}})
                                            {:_as "geometry"
                                             :_relative 1
                                             :mxPoint
                                             [(-> dagre-points
                                                  (first)
                                                  (underscoreify-keys)
                                                  (assoc :_as "sourcePoint"))
                                              (-> dagre-points
                                                  (last)
                                                  (underscoreify-keys)
                                                  (assoc :_as "targetPoint"))]})})))))))]

    {:mxGraphModel
     {:root
      {:mxCell (concat mx-cell-seq mx-edge-seq)}}}))

(defn get-graph-type
  "`graph-object` is the output of setup-mxgraph! or setup-cytograph!
   which would bear the object signaturue of
   `mxGraph` or ``"
  [graph-object]
  (comment (get-graph-type my-mxgraph))
  (case (aget graph-object "constructor" "name")
    "Core" :cytograph
    "mxGraph" :mxgraph
    nil))
