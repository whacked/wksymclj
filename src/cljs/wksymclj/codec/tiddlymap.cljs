(ns wksymclj.codec.tiddlymap
  (:require [cljs.nodejs :as nodejs]
            [wksymclj.codec.tiddlywiki :as tw]
            [clojure.string]
            [wksymclj.nodejs-interface.fileio :as fio]
            [wksymclj.codec.cytoscape :as cyto-codec]
            [wksymclj.codec.graph :as graph-codec]
            [wksymclj.ui.mxgraph :as mx]
            [com.rpl.specter :as spct]
            [cljs-time.core :as time])
  (:require-macros
   [com.rpl.specter :refer [select transform]]
   [swiss.arrows :refer [-<>>]]))


(def $TIDDLYMAP-EDGE-UNKNOWN-TYPE "tmap:unknown")


(defn get-tiddlymap-position-tiddlier-path
  [tiddlers-dir]
  (fio/path-join
   tiddlers-dir
   "$__plugins_felixhayashi_tiddlymap_graph_views_all_map.tid"))

(defn load-tiddlymap-position-info [tiddlers-dir]
  (-> tiddlers-dir
      (get-tiddlymap-position-tiddlier-path)
      (fio/simple-slurp)
      (tw/parse-tid-content)
      (:content)
      (js/JSON.parse)
      (js->clj)))

(defn is-valid-tiddlymap-position-struct? [pos-struct]
  (->> [(or (get pos-struct :x)
            (get pos-struct "x"))
        (or (get pos-struct :y)
            (get pos-struct "y"))]
       (map number?)
       (every? identity)))

(defn save-tiddlymap-position-info [tiddlers-dir position-info]
  {:pre [(->> position-info
              (map (fn [[node-id pos]]
                     (is-valid-tiddlymap-position-struct?
                      pos)))
              (every? identity))]}
  (let [tiddlymap-position-tiddler-path
        (get-tiddlymap-position-tiddlier-path tiddlers-dir)
        
        header-string
        (-> tiddlymap-position-tiddler-path
            (fio/simple-slurp)
            (tw/parse-tid-content)
            (:header)
            (assoc :modified (time/now))
            (tw/render-tid-header))]
    (fio/simple-spit
     tiddlymap-position-tiddler-path
     (str
      header-string
      "\n\n"
      (-> position-info
          (clj->js)
          (js/JSON.stringify nil 2))))))

;; TODO: correct x y offsets for these outputs
(defn get-mxgraph-vertex-position-info [mxgraph-object]
  (-<>> mxgraph-object
        (mx/get-clj-from-mxgraph)
        (get-in <> [:mxGraphModel :root :mxCell])
        (filter (fn [cell]
                  (= "1" (:_vertex cell))))
        (map (fn [cell]
               (let [geom (:mxGeometry cell)]
                 {:name (:_name cell)
                  :x (js/parseInt (:_x geom))
                  :y (js/parseInt (:_y geom))})))))

(defn get-mxgraph-edge-info-for-tiddlymap [mxgraph-object]
  (let [mxgraph-node-id-mapping
        (mx/get-mxgraph-node-id-mapping mxgraph-object)]
   (-<>> mxgraph-object
         (mx/get-clj-from-mxgraph)
         (get-in <> [:mxGraphModel :root :mxCell])
         (filter (fn [cell]
                   (= "1" (:_edge cell))))
         (map (fn [{:keys [_source _target _value]}]
                [(mxgraph-node-id-mapping _source)
                 {:to (mxgraph-node-id-mapping _target)
                  :type (or _value $TIDDLYMAP-EDGE-UNKNOWN-TYPE)}]))
         (remove empty?)
         (merge-with into))))

(defn get-cytograph-node-position-info [cytograph-object]
  (->> cytograph-object
       (cyto-codec/cytoscape-graph-to-data)
       (:elements)
       (:nodes)
       (map (fn [node]
              (println node)
              (let [pos (:position node)]
                ;; :name or :id?
                (assoc (select-keys pos [:x :y])
                       :name (get-in node [:data :name])))))))

(defn get-cytograph-edge-info-for-tiddlymap
  "returns a map of
   { node-name [ {:to target1 :type type1} {:to target2 :type target2} ... ] ...}
   where :type defaults to \"tmap.unknown\" when not provided.
   note that node-name will NOT be in a tmap uuid; you will have to
   xref that in a separate step."
  [cytograph]
  (->> cytograph
       (cyto-codec/cytoscape-graph-to-data)
       (:elements)
       (:edges)
       (map (fn [cyto-edge]
              (let [edge-data (get cyto-edge :data)]
                {(:source edge-data)
                 [{:to (:target edge-data)
                   :type (:type edge-data $TIDDLYMAP-EDGE-UNKNOWN-TYPE)}]})))
       (remove empty?)
       (merge-with into)))
