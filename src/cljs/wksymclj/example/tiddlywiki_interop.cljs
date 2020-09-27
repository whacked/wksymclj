(ns wksymclj.example.tiddlywiki-interop
  (:require [wksymclj.codec.tiddlywiki :as tw]
            [wksymclj.nodejs-interface.fileio :as fio]

            [goog.dom :as gdom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            
            [wksymclj.data-manipulation.graph :as grf]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.codec.graph :as graph-codec]
            [wksymclj.codec.tiddlymap :as tiddlymap]

            [wksymclj.ui.mxgraph :as mx :refer [mxEvent underscoreify-keys]]
            
            [wksymclj.ui.cytoscape :as cyto]

            [wksymclj.ui.browser-interop :as browser]
            [wksymclj.codec.cytoscape :as cyto-codec]

            [wksymclj.codec.org-mode :as wk-org]

            ["cytoscape" :as cytoscape])
   (:require-macros [swiss.arrows :refer [-<> -<>>]]))

(def glob (fio/js-require "glob"))

;; there is a glob.sync method, but let's try to
;; concurrent-ize .org and .tid loaders.
;; sync  (.sync glob path)
;; async (glob path callback)

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
      (rdom/render
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


(def $base-tiddlymap-status-display
  {:auto-save-on-node-position-change? true
   :status-message "initialized"})

(defn render-tiddlymap-status-display!
  [status-ratom
   status-display-el]
  (->> status-display-el
       (rdom/render
        [(fn []
           [:div
            {:style
             {:width "100%"}}
            [:div
             [:input
              {:type "checkbox"
               :checked (get-in @status-ratom [:auto-save-on-node-position-change?])
               :on-change (fn []
                            (swap! status-ratom
                                   update :auto-save-on-node-position-change? not))}]
             "auto save on move node"]
            [:div
             {:style {:width "100%"
                      :line-height "1em"
                      :border "1px solid black"
                      :border-left "none"
                      :border-right "none"}}
             [:span
              {:style {:background "gray"
                       :color "white"
                       :font-size "small"
                       :padding-right "0.2em"
                       :margin "0 0.2em 0 0"}}
              "status"]
             [:code
              (get-in @status-ratom [:status-message])]]])])))

(comment
 
  ;; mxgraph functions no longer work
  (defn get-tiddlymap-positions-from-mxgraph
    [tiddler-db mxgraph]
    (let [x-offset (aget mxgraph "x-offset")
          y-offset (aget mxgraph "y-offset")]
      (->> (mx/get-mxgraph-node-positions mxgraph)
           (map (fn [[node-name pos]]
                  [(get-in tiddler-db
                           [node-name :metadata :tmap.id])
                   {"x" (- (get pos "x")
                           x-offset)
                    "y" (- (get pos "y")
                           y-offset)}]))
           (into {}))))

  (defn setup-mxgraph!
    [tiddlers-dir
     db tiddlymap-pos-info
     graph-container-el
     render-output-el
     render-relevance-el
     status-display-el
     & {:keys [element-renderer]
        :or {element-renderer load-node-content-to-element!}}]
    (comment
      (def $TIDDLYWIKI-TIDDLERS-DIR "/tmp/tiddlers")
      (def file-db (atom {}))
      (load-directory! $TIDDLYWIKI-TIDDLERS-DIR file-db)
      (setup-mxgraph!
       @file-db
       (tiddlymap/load-tiddlymap-position-info $TIDDLYWIKI-TIDDLERS-DIR)
       (gdom/getElement "panel-A")
       (gdom/getElement "panel-C")))
      
    (let [status-display-state (r/atom $base-tiddlymap-status-display)
            
          my-flow-graph
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
                                      (dissoc a :mxPoint))))))
                   cell))
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
             db node-name
             render-output-el
             render-relevance-el))
          (.consume evt)))

      (defn mxgraph-handle-mouse-wheel [evt]
        (if (> 0 (aget evt "deltaY"))
          (.zoomIn my-mxgraph)
          (.zoomOut my-mxgraph))
        (.preventDefault evt))
        
      (render-tiddlymap-status-display!
       status-display-state
       status-display-el)
        
      (doto my-mxgraph
        (.setPanning true)
        (aset "panningHandler" "useLeftButtonForPanning" true)
        (.addListener (aget mxEvent "CLICK")
                      (fn [sender evt]
                        (mxgraph-handle-click sender evt)))
        (.addListener (aget mxEvent "CELLS_MOVED")
                      (fn [sender evt]
                        ;; sender is the mxGraph object
                        (let [cell (aget evt "properties" "cells" 0)]
                          (when (mx/is-vertex? cell)
                            (when (@status-display-state
                                   :auto-save-on-node-position-change?)
                              (->> (get-tiddlymap-positions-from-mxgraph
                                    db my-mxgraph)
                                   (tiddlymap/save-tiddlymap-position-info
                                    tiddlers-dir))
                              (swap! status-display-state
                                     assoc :status-message
                                     (str
                                      (js/Date.)
                                      " saved positions")))))))
        (-> (aget "container" "childNodes" 0)
            (.addEventListener
             "wheel" mxgraph-handle-mouse-wheel)))))

  )

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

(defn setup-cytograph!
  [tiddlers-dir
   db tiddlymap-pos-info
   graph-container-el
   render-output-el
   render-relevance-el
   status-display-el
   & {:keys [element-renderer]
      :or {element-renderer load-node-content-to-element!}}]
  (comment
    (def $TIDDLYWIKI-TIDDLERS-DIR "/tmp/tiddlers")
    (def file-db (atom {}))
    (load-directory! $TIDDLYWIKI-TIDDLERS-DIR file-db)
    (setup-cytograph!
     @file-db
     (load-tiddlymap-position-info $TIDDLYWIKI-TIDDLERS-DIR)
     (gdom/getElement "panel-A")
     (gdom/getElement "panel-C")))

  (let [status-display-state (r/atom $base-tiddlymap-status-display)

        filename->first-header
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
                                              (let [first-node-exists? (has-node? (first edge))
                                                    second-node-exists? (has-node? (second edge))]
                                                (when-not first-node-exists?
                                                  (js/console.warn
                                                   (str "could not find left node: "
                                                        (first edge) "\nin\n"
                                                        edge)))
                                                (when-not second-node-exists?
                                                  (js/console.warn
                                                   (str "could not find right node: "
                                                        (second edge) "\nin\n"
                                                        edge)))
                                                (and first-node-exists?
                                                     second-node-exists?))))
                                    (map cyto-codec/flowgraph-to-cytoscape-edge))
                        }

             :layout {:name "preset" ;; "cose"
                      }
             :style [{:selector "node"
                      :style {:content "data(label)"}}
                     {:selector "edge"
                      :style {:curve-style "bezier"
                              :target-arrow-shape "triangle"}}]
             })))
        
        dragging-node (atom nil)
        ]
    
    
    (render-tiddlymap-status-display!
     status-display-state
     status-display-el)
    
    (doto my-cytograph
      (.on "tap" "node"
           (fn [evt]
             (let [node (aget evt "target")
                   node-id (js-invoke node "id")]
               (element-renderer db node-id
                                 render-output-el
                                 render-relevance-el))))
      
      (.on "position"
           (fn [evt]
             (reset! dragging-node (aget evt "target"))))

      (.on "free"
           (fn [evt]
             (when @dragging-node
               (let [node @dragging-node]
                 (when (@status-display-state
                        :auto-save-on-node-position-change?)
                   (->> (get-tiddlymap-positions-from-cytograph
                         db my-cytograph)
                        (tiddlymap/save-tiddlymap-position-info
                         tiddlers-dir))
                   (swap! status-display-state
                          assoc :status-message
                          (str
                           (js/Date.)
                           " saved positions")))))
             (reset! dragging-node nil))))))

(defn render-tiddlywiki-tags-edges! [db graph-object]

  (comment
    (render-tiddlywiki-tags-edges! @file-db my-cytograph))
  
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
      (if-let [metadata-tags (get-in source-entry [:metadata :tags])]
        (doseq [tag metadata-tags]
          (when-let [target-id (get title-map tag)]
            ;; (str source-id " --(" tag ")--> " target-id)
            (case (graph-codec/get-graph-type graph-object)

              ;; :mxgraph (let [source-node (get-mx-node graph-object source-id)
              ;;                target-node (get-mx-node graph-object target-id)]
              ;;            (mx/add-edge! graph-object
              ;;                          source-node target-node
              ;;                          "tagged with"
              ;;                          {:strokeWidth 2
              ;;                           :strokeColor "green"
              ;;                           :labelBackgroundColor "yellow"
              ;;                           :dashed true}))
              
              :cytograph
              (cyto/add-edge!
               graph-object
               source-id
               target-id
               {:data {:type "tagged with"}
                :style {:content "tagged with"
                        :width 2
                        :line-color "green"
                        :line-style "dashed"
                        :target-arrow-color "green"}}))))))))

(defn save-tiddler!
  "trinary takes tiddler-dir, relpath, and data;
   binary takes abspath to tiddler, and data"
  ([tiddlers-dir tiddler-path parsed-tid]
   (save-tiddler!
    (fio/path-join
     tiddlers-dir tiddler-path) parsed-tid))
  ([tiddler-path parsed-tid]
   (->> parsed-tid
        (tw/render-tid)
        (fio/simple-spit tiddler-path))))

(defn get-tiddlymap-node-name-mapping [tiddler-db]
  (->> tiddler-db
       (map (fn [[node-name node-data]]
              (if-let [tmap-id (get-in node-data [:metadata :tmap.id])]
                [[node-name tmap-id]
                 [tmap-id node-name]])))
       (apply concat)
       (into {})))

(defn setup-graph-tiddlymap-edge-editor!
  [tiddlers-dir graph-object tiddler-db-atom target-el]
  
  (defrecord EdgeActionStatus
      [source-node-name target-node-name
       edge-type-current edge-type-input])
  (def selected-edges (r/atom {}))

  (defn mxgraph-detect-edge-ready-selection [mx-graph]
    (reset! selected-edges {})
    (let [maybe-selection
          (-> mx-graph
              (.getSelectionCells)
              (array-seq))]
      (cond (and (= 1 (count maybe-selection))
                 (some-> maybe-selection
                         (first)
                         (aget "edge")))
            (let [edge-data (first maybe-selection)
                  source (aget edge-data "source")
                  target (aget edge-data "target")
                  source-id (aget source "name")
                  target-id (aget target "name")]
              (swap! selected-edges
                     assoc
                     [source-id target-id]
                     (EdgeActionStatus.
                      source-id target-id
                      (aget edge-data "value")
                      nil)))

            (= 2 (count maybe-selection))
            (let [source (first maybe-selection)
                  target (second maybe-selection)
                  source-id (aget source "name")
                  target-id (aget target "name")]
              (swap! selected-edges
                     assoc
                     [source-id target-id]
                     (EdgeActionStatus.
                      source-id target-id
                      (if-let [existing-edge
                               (mx/get-matching-edge
                                mx-graph
                                (aget source "value")
                                (aget target "value"))]
                        (aget existing-edge "value")
                        nil)
                      nil))))))
  
  (defn setup-mxgraph-detect-edge-selection! [mx-graph]
    (.addListener
     mx-graph
     (aget mxEvent "CLICK")
     (fn [sender evt]
       (mxgraph-detect-edge-ready-selection mx-graph))))

  (defn cytograph-detect-edge-ready-selection [cytograph]
    (reset! selected-edges {})
    (let [maybe-selection
          (-> cytograph
              (js-invoke "$" ":selected"))]
      (cond (and (= 1 (aget maybe-selection "length"))
                 (= (aget maybe-selection 0 "_private" "group")
                    "edges"))
            ;; 1 edge selected
            (let  [edge-data (aget maybe-selection 0 "_private" "data")
                   source-id (aget edge-data "source")
                   target-id (aget edge-data "target")]
              (swap! selected-edges
                     assoc
                     [source-id target-id]
                     (EdgeActionStatus.
                      source-id target-id
                      (or (aget edge-data "type")
                          tiddlymap/$TIDDLYMAP-EDGE-UNKNOWN-TYPE)
                      nil)))
            
            (= 2 (aget maybe-selection "length"))
            ;; 2 nodes selected
            (doseq [[i-first i-second]
                    [[0 1]
                     [1 0]]]
              (let [source-id (aget maybe-selection i-first
                                    "_private" "data" "id")
                    target-id (aget maybe-selection i-second
                                    "_private" "data" "id")
                    maybe-edge (cyto/get-edge cytograph source-id target-id)]
                (swap! selected-edges
                       assoc
                       [source-id target-id]
                       (EdgeActionStatus.
                        source-id target-id
                        (if (= 1 (aget maybe-edge "length"))
                          (aget maybe-edge 0
                                "_private" "data" "type")
                          nil)
                        nil))))

            :else
            (do
              (js/console.log maybe-selection)))))
  
  (defn setup-cytograph-detect-edge-selection! [cytograph]
    (doto cytograph
      (.on "select"
           (fn [_evt]
             (cytograph-detect-edge-ready-selection cytograph)))
      (.on "unselect"
           (fn [_evt]
             (reset! selected-edges {})))))

  (case (graph-codec/get-graph-type graph-object)
    ;; :mxgraph (setup-mxgraph-detect-edge-selection! graph-object)

    :cytograph (setup-cytograph-detect-edge-selection! graph-object))

  (let [tiddlymap-node-name-mapping
        (get-tiddlymap-node-name-mapping
         @tiddler-db-atom)]
    (->> target-el
         (rdom/render
          [(fn []
             [:div
              {:style {:width "100%"}}
              (->> (keys @selected-edges)
                   (map
                    (fn [[source-id target-id]]
                      (let [link-action-status
                            (r/cursor selected-edges
                                      [[source-id target-id]])]
                        [:div
                         [:div
                          {:style {:display "flex"}}
                          [:div
                           {:style {:flex 1}}
                           (if-let [source-node-name
                                    (:source-node-name @link-action-status)]
                             [:div
                              [:div
                               source-node-name]
                              [:div
                               {:style {:font-size "x-small"}}
                               (tiddlymap-node-name-mapping source-node-name)]]
                             "choose source node")]
                          [:div
                           {:style {:flex 1}}
                           [:div
                            [:input
                             {:type "text"
                              :value (:edge-type-input @link-action-status)
                              :on-change (fn [evt]
                                           (swap!
                                            link-action-status
                                            assoc
                                            :edge-type-input
                                            (aget evt "target" "value")))}]]
                           (if-let [cur-type (:edge-type-current @link-action-status)]
                             [:div {:style {:border "1px solid blue"}}
                              cur-type]
                             [:div])]
                          [:div
                           {:style {:flex 1}}
                           (if-let [target-node-name
                                    (:target-node-name @link-action-status)]
                             [:div
                              [:div
                               target-node-name]
                              [:div
                               {:style {:font-size "x-small"}}
                               (tiddlymap-node-name-mapping target-node-name)]]
                             "choose target node")]]
                         [:div
                          (when (:edge-type-input @link-action-status)
                            [:button
                             {:type "button"
                              :on-click (fn [_evt]
                                          (let [edge-type (:edge-type-input @link-action-status)]
                                            (case (graph-codec/get-graph-type graph-object)
                                              ;; :mxgraph (mx/add-edge! graph-object
                                              ;;                        (mx/get-matching-node
                                              ;;                         graph-object {:name source-id})
                                              ;;                        (mx/get-matching-node
                                              ;;                         graph-object {:name target-id})
                                              ;;                        edge-type)

                                              :cytograph
                                              (cyto/add-edge! graph-object
                                                              source-id
                                                              target-id
                                                              {:data
                                                               {:type edge-type}
                                                               :style {:content edge-type}}))
                                            (let [tmap-target-id (tiddlymap-node-name-mapping target-id)
                                                  source-tid-path (-> (@tiddler-db-atom source-id)
                                                                      (:path))
                                                  source-parsed-tid (-> source-tid-path
                                                                        (fio/simple-slurp)
                                                                        (tw/parse-tid-content))]
                                              (when-not (tiddlymap/edge-type-exists?
                                                         tiddlers-dir edge-type)
                                                (tiddlymap/add-tiddlymap-edge-type!
                                                 tiddlers-dir edge-type))
                                              (->> (tiddlymap/update-tiddlymap-edges-for-tiddler
                                                    source-parsed-tid
                                                    {:type edge-type
                                                     :to tmap-target-id})
                                                   (save-tiddler!
                                                    source-tid-path))
                                              (swap! tiddler-db-atom assoc
                                                     source-id (load-file-info source-tid-path)))))}
                             "add"])
                          (when (:edge-type-current @link-action-status)
                            [:button
                             {:type "button"
                              :on-click (fn [_evt]
                                          (case (graph-codec/get-graph-type graph-object)
                                            ;; :mxgraph (mx/remove-edge! graph-object
                                            ;;                           (aget
                                            ;;                            (mx/get-matching-node
                                            ;;                             graph-object {:name source-id})
                                            ;;                            "value")
                                            ;;                           (aget
                                            ;;                            (mx/get-matching-node
                                            ;;                             graph-object {:name target-id})
                                            ;;                            "value"))
                                          
                                            :cytograph
                                            (cyto/remove-edge! graph-object source-id target-id))
                                         
                                          (let [tmap-target-id (tiddlymap-node-name-mapping target-id)
                                                source-tid-path (-> (@tiddler-db-atom source-id)
                                                                    (:path))
                                                source-parsed-tid (-> source-tid-path
                                                                      (fio/simple-slurp)
                                                                      (tw/parse-tid-content))]
                                            (if-let [existing-edge-id
                                                     (some-> (tiddlymap/get-existing-tiddlymap-edge
                                                              source-parsed-tid
                                                              {:type (:edge-type-current @link-action-status)
                                                               :to tmap-target-id})
                                                             (first))]
                                              (do
                                                (->> (update-in
                                                      source-parsed-tid
                                                      [:header :tmap.edges]
                                                      (fn [tmap-edges]
                                                        (dissoc tmap-edges existing-edge-id)))
                                                     (save-tiddler! source-tid-path))
                                                (swap! tiddler-db-atom assoc
                                                       source-id (load-file-info source-tid-path)))
                                              (println "WARNING: NO EDGE FOUND FOR"
                                                       [source-id target-id]))))}
                             "delete"])]]))))])]))))
