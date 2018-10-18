(ns wksymclj.example.tiddlywiki-interop
  (:require [cljs.nodejs :as nodejs]
            [wksymclj.codec.tiddlywiki :as tw]
            [wksymclj.nodejs-interface.fileio :as fio]

            [goog.dom :as gdom]
            [reagent.core :as r]
            
            [wksymclj.data-manipulation.graph :as grf]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.codec.graph :as graph-codec]
            [wksymclj.codec.tiddlymap :as tiddlymap]

            [wksymclj.ui.mxgraph :as mx
             :refer [underscoreify-keys]]
            [wksymclj.ui.cytoscape :as cyto]

            [wksymclj.ui.browser-interop :as browser]
            [wksymclj.codec.cytoscape :as cyto-codec]

            [wksymclj.codec.org-mode :as wk-org])
   (:require-macros [swiss.arrows :refer [-<> -<>>]]))

;; there is a glob.sync method, but let's try to
;; concurrent-ize .org and .tid loaders.
;; sync  (.sync glob path)
;; async (glob path callback)
(def glob (nodejs/require "glob"))

;; this assumes the mx files are included in the html!
(def mxEvent js/mxEvent)

(def cytoscape (nodejs/require "cytoscape"))

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

;; build the flow-graph
(defn file-db-to-flow-graph
  "loads flow graph information from a preloaded file db map
   it looks for .tid files that contain tiddlymap properties:
   - tmap.id
   - tmap.edges
  "
  [db]
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

(defn get-file-name-to-first-header-mapping
  ;; TODO: generalize this to beyond tiddlers
  "parses files within db to build a
   filename -> first-header
   mapping; first-header is a proxy of main excerpt of interest"
  [db]
  (->> db
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
       (into {})))

(defn load-node-content-to-element! [db node-id element]
  (js/console.info "loading: " node-id "...")
  (if-let [node-data (db node-id)]
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
            (wk-org/orga-org->hiccup (fn [content]
                                       ;; arbitrary preprocessor
                                       content)))]
       element)
      (js/console.log "done: " node-id))
    (js/console.warn (str "could not get data for: " node-id))))

(defn setup-mxgraph!
  [db tiddlymap-pos-info
   graph-container-el render-output-el
   & {:keys [element-renderer]
      :or {element-renderer load-node-content-to-element!}}]
  (comment
    (setup-mxgraph!
     @file-db
     (tiddlymap/load-tiddlymap-position-info $TIDDLYWIKI-TIDDLERS-DIR)
     (gdom/getElement "panel-A")
     (gdom/getElement "panel-C")))
  
  (let [my-flow-graph
        (-> (file-db-to-flow-graph db)
            (update :node-list
                    (fn [node-list]
                      (->> node-list
                           (map (partial
                                 merge {:width 100
                                        :height 40}))))))
        
        mx-id2name
        (->> (dagre/get-node-id-mapping
              (:node-list my-flow-graph)
              2)
             (map (fn [[k v]] [v k]))
             (into {}))

        filename->first-header
        (get-file-name-to-first-header-mapping db)
        
        my-mxgraph
        (let [get-adjust (fn [which]
                           (->> tiddlymap-pos-info
                                (map (fn [[_ m]]
                                       (m which)))
                                (apply Math/min)
                                (Math/abs)))

              base-padding 10
              adj-x (+ (get-adjust "x") base-padding)
              adj-y (+ (get-adjust "y") base-padding)
              
              get-position-info (fn [node-name]
                                  (when-let [file-info (db node-name)]
                                    (let [tmap-id (get-in file-info [:metadata :tmap.id])]
                                      (tiddlymap-pos-info tmap-id))))]
          
          (doto graph-container-el
            (browser/set-element-style!
             {:overflow "scroll"
              :border "2px solid red"
              }))
          
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
                                          (db node-name)
                                          [:metadata :title])))]
                   (if-let [pos (get-position-info node-name)]
                     (-> cell
                         (assoc :_name node-name)
                         (assoc :_value
                                (or override-label node-name))
                         (assoc-in [:mxGeometry :_x] (+ (pos "x") adj-x))
                         (assoc-in [:mxGeometry :_y] (+ (pos "y") adj-y)))
                     ;; remove the initial dagre layout because we are forcibly
                     ;; repositioning the nodes, causing the edge positiongs to
                     ;; be incorrect
                     (if-not (:_edge cell)
                       cell
                       (update-in cell [:mxGeometry :Array]
                                  (fn [a]
                                    (dissoc a :mxPoint))))))))
              (mx/render-mxgraph-data-to-element! graph-container-el)
              (doto ((fn [mxgraph]
                       ;; HACK -- store the adjustment into the graph object
                       ;; for later conversion
                       (aset mxgraph "x-offset" adj-x)
                       (aset mxgraph "y-offset" adj-y))))))]

    (defn mxgraph-handle-click
      [sender evt]
      (when-let [cell (.getProperty evt "cell")]
        (when-let [node-name (-> (aget cell "id")
                                 (js/parseInt)
                                 (mx-id2name))]
          (element-renderer
           db node-name render-output-el))
        (.consume evt)))

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
      (.addListener (aget mxEvent "MOVE_END")
                    (fn [sender evt]
                      (js/console.log "MOVE END")
                      (js/console.info js/arguments)
                      (js/console.log
                       sender evt)))
      (-> (aget "container" "childNodes" 0)
          (.addEventListener
           "wheel" mxgraph-handle-mouse-wheel)))))

(defn get-tiddlymap-positions-from-mxgraph
  [tiddler-db mxgraph]
  (let [x-offset (aget mxgraph "x-offset")
        y-offset (aget mxgraph "y-offset")]
    (->> (mx/get-mxgraph-node-positions mxgraph)
         (map (fn [[node-name pos]]
                [(get-in tiddler-db
                         [node-name
                          :metadata
                          :tmap.id])
                 {"x" (- (get pos "x")
                         x-offset)
                  "y" (- (get pos "y")
                         y-offset)}]))
         (into {}))))

(defn setup-cytograph!
  [db tiddlymap-pos-info
   graph-container-el render-output-el
   & {:keys [element-renderer]
      :or {element-renderer load-node-content-to-element!}}]
  (comment
    (setup-cytograph!
     @file-db
     (load-tiddlymap-position-info $TIDDLYWIKI-TIDDLERS-DIR)
     (gdom/getElement "panel-A")
     (gdom/getElement "panel-C")))
  
  (let [filename->first-header
        (get-file-name-to-first-header-mapping db)

        my-cytograph
        (let [cyto-nodes (->> (file-db-to-flow-graph db)
                              (:node-list)
                              (map (fn [node]
                                     (let [pos (some-> (:name node)
                                                       (db)
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
                             (into #{}))]
          (cytoscape
           (clj->js
            {:container graph-container-el
             :elements {:nodes cyto-nodes
                        :edges (->> (file-db-to-flow-graph db)
                                    (:edge-list)
                                    (filter (fn [edge]
                                              (or (has-node? (first edge))
                                                  (has-node? (second edge)))))
                                    (map cyto-codec/flowgraph-to-cytoscape-edge))}

             :layout {:name "preset" ;; "cose"
                      }
             :style [{:selector "node"
                      :style {:content "data(label)"}}
                     {:selector "edge"
                      :style {:curve-style "bezier"
                              :target-arrow-shape "triangle"}}]
             })))]
    (doto my-cytograph
      (.on "tap" "node"
           (fn [evt]
             (let [node (aget evt "target")
                   node-id (js-invoke node "id")]
               (element-renderer
                db node-id render-output-el)))))))

(defn get-tiddlymap-positions-from-cytograph
  [tiddler-db cytograph]
  (->> (get-in
        (cyto-codec/cytoscape-graph-to-data cytograph)
        [:elements :nodes])
       (map (fn [node]
              (let [node-name (get-in node [:data :id])]
                [(get-in tiddler-db
                         [node-name :metadata :tmap.id])
                 (:position node)])))
       (remove (fn [[k _]] (nil? k)))
       (into {})))

(defn render-tiddlywiki-tags-edges! [db graph-object]
  ;; add tiddlywiki tag edges
  (let [title-map (->> db
                       (map (fn [[k m]]
                              (if-let [title (get-in m [:metadata :title])]
                                [title k])))
                       (remove empty?)
                       (into {}))
        get-mx-node (memoize (fn [G node-id]
                               (mx/get-matching-cell
                                G {:name node-id})))]
    (doseq [[source-id source-entry] db]
      (if-let [metadata-tags (-> (get-in source-entry [:metadata :tags])
                                 (clojure.string/split #"\s+"))]
        (doseq [tag metadata-tags]
          (when-let [target-id (get title-map tag)]
            ;; (println (str source-id " --(" tag ")--> " target-id))
            (case (graph-codec/get-graph-type graph-object)
              :mxgraph
              (let [source-node (get-mx-node graph-object source-id)
                    target-node (get-mx-node graph-object target-id)]
                (mx/add-edge! graph-object
                              source-node target-node
                              "tagged with"
                              {:strokeWidth 2
                               :strokeColor "green"
                               :labelBackgroundColor "yellow"
                               :dashed true}))
              
              :cytograph
              (cyto/add-edge!
               graph-object
               source-id
               target-id
               {:style {:content "tagged with"
                        :width 2
                        :line-color "green"
                        :line-style "dashed"
                        :target-arrow-color "green"}}))))))))

(comment
  (do
    
    (def $TIDDLYWIKI-TIDDLERS-DIR "/tmp/tiddlers")
    
    (def file-db (atom {}))
    
    (load-directory! $TIDDLYWIKI-TIDDLERS-DIR file-db)

    ;; (def my-mxgraph
    ;;   (setup-mxgraph!
    ;;    @file-db
    ;;    (gdom/getElement "panel-A")
    ;;    (gdom/getElement "panel-C")))

    (def my-cytograph
      (setup-cytograph!
       @file-db
       (load-tiddlymap-position-info $TIDDLYWIKI-TIDDLERS-DIR)
       (gdom/getElement "panel-A")
       (gdom/getElement "panel-C")))

    (render-tiddlywiki-tags-edges! @file-db my-cytograph)))
