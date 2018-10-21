(ns wksymclj.ui.cytoscape
  (:require
   [wksymclj.codec.cytoscape :as cyto-codec]))

(defn add-node!
  "(add-node!
    cyto-graph
    {:id \"my-new-node\"}
    {:position {:x -300 :y -300}})
  "
  [cyto-graph node-data & [extra-data]]
  (.add cyto-graph (clj->js {:nodes [(merge
                                      {:data node-data}
                                      extra-data)]})))

(defn remove-node! [cyto-graph node-id]
  (let [matching-nodes (.elements
                        cyto-graph
                        (str "node[id=\"" node-id "\"]"))]
    (when (< 0 (aget matching-nodes "length"))
      (.remove cyto-graph matching-nodes)
      true)))

(defn add-edge!
  "(add-edge!
      cyto-graph
      source-id
      target-id
      {:style {:width 12
               :content \"my label\"
               :line-color \"green\"
               :source-label \"at the source\"
               :target-label \"at the target\"}})"
  [cyto-graph source-id target-id & [extra-data]]
  (.add cyto-graph
        (clj->js {:edges [(merge-with
                           merge
                           {:data {:source source-id
                                   :target target-id}}
                           extra-data)]})))

(defn remove-edge! [cyto-graph source-id target-id]
  (let [matching-edges
        (.elements
         cyto-graph
         (str "edge"
              "[source=\"" source-id "\"]"
              "[target=\"" target-id "\"]"))]
    (when (< 0 (aget matching-edges "length"))
      (.remove cyto-graph matching-edges)
      true)))

(defn set-element-style! [cyto-element style-map]
  (.style cyto-element (clj->js style-map)))

(defn get-node [cyto-graph node-id]
  (.elements cyto-graph (str "node[name=\"" node-id "\"]")))

(defn get-edge [cyto-graph source-id target-id]
  (.elements
   cyto-graph
   (str "edge"
        "[source=\"" source-id "\"]"
        "[target=\"" target-id "\"]")))

(defn get-node-position [node]
  (if (map? node)
    (let [pos (:position node)]
      ;; :name or :id?
      (assoc (select-keys pos [:x :y])
             :name (get-in node [:data :name])))
    ;; assume js cyto node object
    (-> node
        (js-invoke "position")
        (js->clj :keywordize-keys true))))

(defn get-graph-node-position-coll [cytograph-object]
  (->> cytograph-object
       (cyto-codec/cytoscape-graph-to-data)
       (:elements)
       (:nodes)
       (map get-node-position)))
