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


(def uuidv4 (nodejs/require "uuid/v4"))

(def $TIDDLYMAP-EDGE-UNKNOWN-TYPE "tmap:unknown")
(def $TIDDLYMAP-FILE-PREFIX "$__plugins_felixhayashi_tiddlymap_")
(def $TIDDLYMAP-EDGETYPES-FILE-PREFIX
  (str $TIDDLYMAP-FILE-PREFIX "graph_edgeTypes_"))
(def $TIDDLYMAP-EDGETYPES-INTERNAL-PREFIX
  "$:/plugins/felixhayashi/tiddlymap/graph/edgeTypes")

(defn get-tiddlymap-position-tiddlier-path
  [tiddlers-dir]
  (fio/path-join
   tiddlers-dir
   (str $TIDDLYMAP-FILE-PREFIX
        "graph_views_all_map.tid")))

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

(defn get-existing-tiddlymap-edge
  "given a parsed tid datastructure
   (see wksymclj.codec.tiddlywiki/parse-tid-content),
   look for an edge with matching :to and :type.

   target-edge-info is the same data that is used for :tmap.edges, e.g.
   {:type \"tmap:unknown\" :to \"blah.tid\"}

   if an id mapper is found, use it to derive the tmap id (uuid);
   else assume the ids are valid as given."
  ([parsed-tid target-edge-info]
   (get-existing-tiddlymap-edge
    parsed-tid target-edge-info identity))
  ([parsed-tid target-edge-info id-mapper]
   (-<>> (get-in parsed-tid [:header :tmap.edges])
         (filter (fn [[tmap-edge-id edge-info]]
                   (and (= (get edge-info "to")
                           (id-mapper (or (:to target-edge-info)
                                          (target-edge-info "to"))))
                        (= (get edge-info "type")
                           (or (:type target-edge-info)
                               (target-edge-info "type"))))))
         (first))))

(defn update-tiddlymap-edges-for-tiddler
  "given a parsed tid datastructure
   (see wksymclj.codec.tiddlywiki/parse-tid-content)

   if an existing :to and :type is found, do nothing;
   else add a new edge to the tmap.edges data, returning
   the updated tiddler structure"
  ([parsed-tid new-edge-info]
   (update-tiddlymap-edges-for-tiddler
    parsed-tid new-edge-info identity))
  ([parsed-tid new-edge-info id-mapper]

   (comment
     ;; count will increase if new edge;
     ;; else it will stay the same
     (-> (update-tiddlymap-edges-for-tiddler
          (tw/parse-tid-content tw-string)
          {:type "tmap:unknown"
           :to "new-tiddler.tid"}
          {"new-tiddler.tid" "some-random-uuid4"})
         (get-in [:header :tmap.edges])
         (count)))
   
   (if-let [match (get-existing-tiddlymap-edge
                   parsed-tid new-edge-info id-mapper)]
     ;; existing match found with id (first match)
     parsed-tid
     ;; create new edge using uuidv4
     (let [new-edge-id (uuidv4)]
       (update-in parsed-tid
                  [:header :tmap.edges]
                  (fn [current-edges]
                    (assoc current-edges
                           new-edge-id
                           {"to" (id-mapper (:to new-edge-info))
                            "type" (:type new-edge-info)})))))))

(defn get-edge-type-filepath [tiddlers-dir edge-type-name]
  (->> (str $TIDDLYMAP-EDGETYPES-FILE-PREFIX
            edge-type-name
            ".tid")
       (fio/path-join tiddlers-dir)))

(defn edge-type-exists? [tiddlers-dir edge-type-name]
  (-> (get-edge-type-filepath
       tiddlers-dir edge-type-name)
      (fio/path-exists?)))

(defn add-tiddlymap-edge-type! [tiddlers-dir edge-type-name]
  (let [edgetype-tid-path (get-edge-type-filepath
                           tiddlers-dir edge-type-name)
        now (time/now)]
    (->> {:header
          {:created now,
           :modified now,
           :title (str $TIDDLYMAP-EDGETYPES-INTERNAL-PREFIX
                       "/" edge-type-name)
           :content ""}}
         (tw/render-tid)
         (fio/simple-spit edgetype-tid-path))))
