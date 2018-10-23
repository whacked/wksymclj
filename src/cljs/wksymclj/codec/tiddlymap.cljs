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

(defn get-relpath-tmap-id-mapping
  [tiddler-db]
  (->> tiddler-db
       (filter (fn [[_ tid]]
                 (-> (get-in tid [:metadata :tmap.id])
                     (empty?)
                     (not))))
       (map (fn [[relpath tid]]
              [relpath (get-in tid [:metadata :tmap.id])]))
       (map (fn [pair]
              [pair (vec (reverse pair))]))
       (apply concat)
       (into {})))

(defn get-title-tag-tmap-node-mapping
  [tiddler-db tm-node-mapping]
  (->> tiddler-db
       (map (fn [[relpath tid]]
              [(get-in tid [:metadata :title])
               (tm-node-mapping relpath)]))
       (into {})))

(defn get-edge-mapping
  "returns map of [tmap.id_source tmap.id_target] -> tmap.type"
  [tiddler-db]
  (->> tiddler-db
       (map (fn [[relpath {:keys [metadata]}]]
              (->> (:tmap.edges metadata)
                   (map (fn [[edge-id edge-info]]
                          {[(:tmap.id metadata) (get edge-info "to")]
                           [(get edge-info "type")]}))
                   (apply merge-with concat))))
       (into {})))

(defn find-all-orphan-tiddler-names
  "find all nodes that have no links (tags or edges)"
  [tiddlywiki-tiddlers-dir tiddler-db]

  (let [tm-position-map
        (load-tiddlymap-position-info tiddlywiki-tiddlers-dir)

        tm-node-mapping (get-relpath-tmap-id-mapping tiddler-db)

        tm-tag-node-mapping (get-title-tag-tmap-node-mapping
                             tiddler-db tm-node-mapping)

        tm-tag-members (->> tiddler-db
                            (map (fn [[relpath {:keys [metadata]}]]
                                   (->> (:tags metadata)
                                        (map (fn [tag]
                                               (when-let [target-tmap-id
                                                          (tm-tag-node-mapping tag)]
                                                 [(tm-node-mapping relpath)
                                                  target-tmap-id])))
                                        (remove nil?))))
                            (flatten)
                            (into #{}))

        tm-edge-members (->> tiddler-db
                             (get-edge-mapping)
                             (keys)
                             (flatten)
                             (into #{}))]
    (->> tiddler-db
         (remove (fn [[relpath m]]
                   (-> (tm-node-mapping relpath)
                       (tm-tag-members))))
         (remove (fn [[relpath m]]
                   (let [tmap-id (tm-node-mapping relpath)]
                     (tm-edge-members tmap-id))))
         (map (fn [[relpath _]]
                relpath)))))
