(ns wksymclj.example.tiddlywiki-interop
  (:require [cljs.nodejs :as nodejs]
            [wksymclj.codec.tiddlywiki :as tw]
            [wksymclj.nodejs-interface.fileio :as fio]

            [goog.dom :as gdom]
            [reagent.core :as r]
            
            [wksymclj.data-manipulation.graph :as grf]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.codec.graph :as graph-codec]

            [wksymclj.ui.mxgraph :as mx
             :refer [underscoreify-keys]]

            [wksymclj.ui.browser-interop :as browser]
            [wksymclj.codec.cytoscape :as cyto-codec])
   (:require-macros [swiss.arrows :refer [-<> -<>>]]))

;; there is a glob.sync method, but let's try to
;; concurrent-ize .org and .tid loaders.
;; sync  (.sync glob path)
;; async (glob path callback)
(def glob (nodejs/require "glob"))

(def cytoscape (nodejs/require "cytoscape"))

(def file-db (atom {}))

(defn load-directory! [tiddlers-dir db-atom]
  (doseq [extension ["org" "tid"]]
    (glob (fio/path-join tiddlers-dir (str "**/*." extension))
          (fn [err matches]
            (when err
              (throw err))
            (->> matches
                 (array-seq)
                 (mapv (fn [path]
                         (let [file-name (clojure.string/replace
                                          path (re-pattern
                                                (str "^" tiddlers-dir "/"))
                                          "")]
                           (case extension
                             "tid"
                             (when-let [tid (-> path
                                                (fio/simple-slurp)
                                                (tw/parse-tid-content))]
                               [file-name {:path path
                                           :metadata (:header tid)
                                           :content (:content tid)}])

                             "org"
                             [file-name {:path path
                                         :content (fio/simple-slurp path)}]
                             
                             nil))))
                 (remove nil?)
                 (into {})
                 (reset! db-atom))))))

(defn load-tiddlymap-position-info [tiddlers-dir]
  (-> (fio/path-join
       tiddlers-dir
       "$__plugins_felixhayashi_tiddlymap_graph_views_all_map.tid")
      (fio/simple-slurp)
      (tw/parse-tid-content)
      (:content)
      (js/JSON.parse)
      (js->clj)))

;; build the flow-graph
(defn file-db-to-flow-graph [db]
  (let [tid-files (->> db
                       (filter (fn [[k m]]
                                 (clojure.string/ends-with? k ".tid"))))
        tmap-id-mapping (->> tid-files
                             (map (fn [[k m]]
                                    (if-let [tmap-id (get-in m [:metadata :tmap.id])]
                                      [tmap-id k])))
                             (remove empty?)
                             (into {}))]
    (loop [remain tid-files
           node-list []
           edge-list []]
      (if (empty? remain)
        {:node-list node-list
         :edge-list edge-list}
        (let [[k m] (first remain)
              md (:metadata m)
              tmap-edges (:tmap.edges md)]
          (recur (rest remain)
                 (conj node-list (merge
                                  {:name k
                                   :label (:title md)}))
                 (concat edge-list (->> tmap-edges
                                        (map (fn [[edge-id edge-mapping]]
                                               (let [target-tmap-id (edge-mapping "to")
                                                     target-name (tmap-id-mapping target-tmap-id)]
                                                 [k
                                                  target-name
                                                  {:label (edge-mapping "type")}])))
                                        (remove empty?)))))))))

(comment
  
  (load-directory! "/path/to/tiddlers" file-db)

  (def my-mxgraph
    (let [my-flow-graph (-> (file-db-to-flow-graph @file-db)
                            (update :node-list
                                    (fn [node-list]
                                      (->> node-list
                                           (map (partial
                                                 merge {:width 80
                                                        :height 30}))))))

          get-adjust (fn [which]
                       (->> tiddlymap-pos-info
                            (map (fn [[_ m]]
                                   (m which)))
                            (apply Math/min)
                            (Math/abs)))

          adj-x (get-adjust "x")
          adj-y (get-adjust "y")
          
          id2name (->> (dagre/get-node-id-mapping
                        (:node-list my-flow-graph)
                        2)
                       (map (fn [[k v]] [v k]))
                       (into {}))
          get-position-info (fn [node-name]
                              (when-let [file-info (@file-db node-name)]
                                (let [tmap-id (get-in file-info [:metadata :tmap.id])]
                                  (tiddlymap-pos-info tmap-id))))
          $target-el (gdom/getElement "panel-A")]
      (doto $target-el
        (browser/set-element-style!
         {:overflow "scroll"
          :border "2px solid red"}))
      
      (-> (dagre/make-dagre
           (:node-list my-flow-graph)
           (:edge-list my-flow-graph))
          (graph-codec/dagre-graph-to-mxgraph-data)
          (mx/transform-cells-in-mxgraph
           (fn [cell]
             (if-let [pos (-> cell
                              (:_id)
                              (id2name)
                              (get-position-info))]
               (-> cell
                   (assoc-in [:mxGeometry :_x] (+ (pos "x") adj-x 10))
                   (assoc-in [:mxGeometry :_y] (+ (pos "y") adj-y 10)))
               ;; remove the initial dagre layout because we are forcibly
               ;; repositioning the nodes, causing the edge positiongs to
               ;; be incorrect
               (if-not (:_edge cell)
                 cell
                 (update-in cell [:mxGeometry :Array]
                            (fn [a]
                              (dissoc a :mxPoint)))))))
          (mx/render-mxgraph-data-to-element! $target-el)))))

(comment
  (def my-cytograph
    (let [$target-el (gdom/getElement "panel-A")

          cyto-nodes (->> (file-db-to-flow-graph @file-db)
                          (:node-list)
                          (map (fn [node]
                                 (if-let [pos (some-> (:name node)
                                                      (@file-db)
                                                      (get-in [:metadata :tmap.id])
                                                      (tiddlymap-pos-info))]
                                   (merge (clojure.walk/keywordize-keys pos)
                                          node)
                                   node)))
                          (map cyto-codec/flowgraph-to-cytoscape-node))
          has-node? (->> cyto-nodes
                         (map (fn [cnode]
                                (get-in cnode [:data :id])))
                         (into #{}))
          cyto-data {:container $target-el
                     :elements {:nodes cyto-nodes
                                :edges (->> (file-db-to-flow-graph @file-db)
                                            (:edge-list)
                                            (filter (fn [edge]
                                                      (or (has-node? (first edge))
                                                          (has-node? (second edge)))))
                                            (map cyto-codec/flowgraph-to-cytoscape-edge))}

                     :layout {:name "cose"}
                     :style [{:selector "node"
                              :style {:content "data(id)"}}]}]
      (cytoscape (clj->js cyto-data)))))
