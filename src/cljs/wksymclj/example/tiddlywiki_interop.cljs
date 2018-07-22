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
            [wksymclj.ui.cytoscape :as cyto]

            [wksymclj.ui.browser-interop :as browser]
            [wksymclj.codec.cytoscape :as cyto-codec]

            [wksymclj.codec.org-mode :as wk-org])
   (:require-macros [swiss.arrows :refer [-<> -<>>]]))

(def $TIDDLYWIKI-TIDDLERS-DIR "/tmp/tiddlers")

;; there is a glob.sync method, but let's try to
;; concurrent-ize .org and .tid loaders.
;; sync  (.sync glob path)
;; async (glob path callback)
(def glob (nodejs/require "glob"))

(def cytoscape (nodejs/require "cytoscape"))

(def file-db (atom {}))


(defn FileInfoStruct [filepath content]
  {:path filepath
   :content content})

(def file-info-struct-mapping
  {"tid" (fn tid-parser [filepath]
           (when-let [tid (-> filepath
                              (fio/simple-slurp)
                              (tw/parse-tid-content))]
             (-> (FileInfoStruct
                  filepath (:content tid))
                 (assoc :metadata (:header tid)))))
   "raw" (fn general-parser [filepath]
           (FileInfoStruct
            filepath
            (fio/simple-slurp filepath)))})

(defn get-file-info-struct-parser [extension]
  (file-info-struct-mapping
   extension
   (file-info-struct-mapping "raw")))

(defn load-file-info [file-path]
  (let [extension (subs (fio/get-extension file-path) 1)
        parser (get-file-info-struct-parser extension)]
    (parser file-path)))

(defn cache-file-info! [db-atom repo-dir file-path]
  (let [file-name (fio/get-relative-path
                   repo-dir file-path)]
    (swap! db-atom
           update file-name
           (fn [x]
             (load-file-info file-path)))))

(defn load-directory!
  "populates with mapping of
   `file-name` -> { :path `file-path`
                    :metadata `various`
                    :content `raw` }"
  [tiddlers-dir db-atom]
  (doseq [extension ["org" "tid"]]
    (glob (fio/path-join tiddlers-dir (str "**/*." extension))
          (fn [err matches]
            (when err
              (throw err))
            (->> matches
                 (array-seq)
                 ;; filter out tiddlywiki internal / plugin tiddlers
                 ;; e.g. tiddlers/$__plugins_felixhayashi_vis.tid
                 ;;      tiddlers/$__grouped.tid ... etc
                 (filter (fn [path]
                           (not (re-find #"[/\\]?\$__.+\.tid" path))))
                 (mapv (fn [path]
                         (let [file-name (fio/get-relative-path
                                          tiddlers-dir path)]
                           [file-name (load-file-info path)])))
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

  (load-directory! $TIDDLYWIKI-TIDDLERS-DIR file-db)

  (def tiddlymap-pos-info
    (load-tiddlymap-position-info $TIDDLYWIKI-TIDDLERS-DIR))

  (def filename->first-header
    (->> @file-db
         ;; get the anonymous tiddlers
         (filter (fn [[fname fdata]]
                   (and (= (get-in fdata [:metadata :type])
                           "text/org")
                        (re-find #"\d{4}-\d{2}-\d{2}[ _]\d{2}[-:]\d{2}[-:]\d{2}"
                                 fname))))
         (map (fn [[fname fdata]]
                [fname (or (-> (:content fdata)
                               (wk-org/org->clj-ast)
                               (wk-org/org-ast-get-first-headline))
                           fname)]))
         (into {}))))

(defn load-node-content-to-element! [node-id element]
  (js/console.info "loading: " node-id "...")
  (if-let [node-data (@file-db node-id)]
    (do
      (r/render
       [:div
        {:style {:width "100%"
                 :height "100%"
                 :overflow "scroll"}}
        [:h3 {:style {:font-family "Monospace"
                      :color "#933"}}
         node-id]
        (-> node-data
            (:content)
            (wk-org/orga-org->hiccup))]
       element)
      (js/console.log "done: " node-id))
    (js/console.warn (str "could not get data for: " node-id))))

(comment
  (def my-flow-graph
    (-> (file-db-to-flow-graph @file-db)
        (update :node-list
                (fn [node-list]
                  (->> node-list
                       (map (partial
                             merge {:width 100
                                    :height 40})))))))
  
  (def mx-id2name
    (->> (dagre/get-node-id-mapping
          (:node-list my-flow-graph)
          2)
         (map (fn [[k v]] [v k]))
         (into {})))

  (def my-mxgraph
    (let [get-adjust (fn [which]
                       (->> tiddlymap-pos-info
                            (map (fn [[_ m]]
                                   (m which)))
                            (apply Math/min)
                            (Math/abs)))

          adj-x (get-adjust "x")
          adj-y (get-adjust "y")
          
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
             (let [node-name (-> cell (:_id) (mx-id2name))
                   override-label (or
                                   ;; for anonymous tiddlers
                                   (filename->first-header node-name)
                                   ;; for normal tiddlers
                                   (if node-name
                                     (get-in
                                      (@file-db node-name)
                                      [:metadata :title])))]
               (if-let [pos (get-position-info node-name)]
                 (-> cell
                     (assoc :_name node-name)
                     (assoc :_value
                            (or override-label node-name))
                     (assoc-in [:mxGeometry :_x] (+ (pos "x") adj-x 10))
                     (assoc-in [:mxGeometry :_y] (+ (pos "y") adj-y 10)))
                 ;; remove the initial dagre layout because we are forcibly
                 ;; repositioning the nodes, causing the edge positiongs to
                 ;; be incorrect
                 (if-not (:_edge cell)
                   cell
                   (update-in cell [:mxGeometry :Array]
                              (fn [a]
                                (dissoc a :mxPoint))))))))
          (mx/render-mxgraph-data-to-element! $target-el))))

  (def mxEvent js/mxEvent)
  (let [output-panel (gdom/getElement "panel-C")]
    (defn mxgraph-handle-click
      [sender evt]
      (when-let [cell (.getProperty evt "cell")]
        (when-let [node-name (-> (aget cell "id")
                                 (js/parseInt)
                                 (mx-id2name))]
          (load-node-content-to-element! node-name output-panel))
        (.consume evt))))

  (defn mxgraph-handle-mouse-wheel [evt]
    (if (> 0 (aget evt "deltaY"))
      (.zoomIn my-mxgraph)
      (.zoomOut my-mxgraph))
    (.preventDefault evt))

  (doto my-mxgraph
    (.setPanning true)
    (aset "panningHandler" "useLeftButtonForPanning" true)
    (.addListener (aget mxEvent "CLICK")
                  (fn [sender evt]
                    (mxgraph-handle-click sender evt)))
    (-> (aget "container" "childNodes" 0)
        (.addEventListener "wheel"
                           mxgraph-handle-mouse-wheel))))

(comment
  (def my-cytograph
    (let [$target-el (gdom/getElement "panel-A")

          cyto-nodes (->> (file-db-to-flow-graph @file-db)
                          (:node-list)
                          (map (fn [node]
                                 (let [pos (some-> (:name node)
                                                   (@file-db)
                                                   (get-in [:metadata :tmap.id])
                                                   (tiddlymap-pos-info)
                                                   (clojure.walk/keywordize-keys))
                                       override-label (filename->first-header (:name node))]
                                   (-<>> (if override-label
                                           (assoc node :label override-label)
                                           node)
                                         (merge pos)))))
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

                     :layout {:name "preset"  ;; "cose"
                              }
                     :style [{:selector "node"
                              :style {:content "data(label)"}}
                             {:selector "edge"
                              :style {:curve-style "bezier"
                                      :target-arrow-shape "triangle"}}]
                     
                     }]
      (cytoscape (clj->js cyto-data))))
  
  (.on my-cytograph "tap" "node"
       (fn [evt]
         (let [output-pane (gdom/getElement "panel-B")
               
               node (aget evt "target")
               node-id (js-invoke node "id")]
           (load-node-content-to-element! node-id output-pane)))))

(comment
  ;; add tiddlywiki tag edges
  (let [graph my-cytograph ;; my-mxgraph
        title-map (->> @file-db
                       (map (fn [[k m]]
                              (if-let [title (get-in m [:metadata :title])]
                                [title k])))
                       (remove empty?)
                       (into {}))
        get-mx-node (memoize (fn [G node-id]
                               (mx/get-matching-cell
                                G {:name node-id})))]
    (->> @file-db
         (map (fn [[source-id source-entry]]
                (if-let [metadata-tags (-> (get-in source-entry [:metadata :tags])
                                           (clojure.string/split #"\s+"))]
                  (doseq [tag metadata-tags]
                    (when-let [target-id (get title-map tag)]
                      (println (str source-id " --(" tag ")--> " target-id))
                      (case (aget graph "constructor" "name")
                        "mxGraph"
                        (let [source-node (get-mx-node graph source-id)
                              target-node (get-mx-node graph target-id)]
                          (mx/add-edge! graph
                                        source-node target-node
                                        "tagged with"
                                        {:strokeWidth 2
                                         :strokeColor "green"
                                         :labelBackgroundColor "yellow"
                                         :dashed true}))
                        
                        "Core" ;; cytoscape
                        (cyto/add-edge!
                         graph
                         source-id
                         target-id
                         {:style {:content "tagged with"
                                  :width 2
                                  :line-color "green"
                                  :line-style "dashed"
                                  :target-arrow-color "green"}}))))))))))
