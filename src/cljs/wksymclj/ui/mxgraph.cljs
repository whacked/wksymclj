(ns wksymclj.ui.mxgraph
  (:require [goog.dom :as gdom]
            [cljs.nodejs :as nodejs]

            [wksymclj.nodejs-interface.fileio :as fio]
            [wksymclj.data-manipulation.graph
             :as grf
             :refer [get-edge-midpt
                     state-declaration-to-flow-graph]]
            [wksymclj.ui.browser-interop
             :refer [clear-dom-element!]]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.nodejs-interface.fileio
             :refer [path-join]]
            [wksymclj.data-manipulation.xml
             :refer [xml->js js->xml]]
            [clojure.string]
            [cljs.spec.alpha :as spec]
            [com.rpl.specter :as spct])
  (:require-macros
   [swiss.arrows :refer [-<> -<>>]]
   [com.rpl.specter
    :refer [select transform]]))

(def $DEBUG-LEVEL 0)

(def $this-namespace "wksymclj.ui.mxgraph")

;; mx dependencies
(def mxGraph js/mxGraph)
(def mxRubberband js/mxRubberband)
(def mxUtils js/mxUtils)
(def mxCodec js/mxCodec)
(def mxConstants js/mxConstants)
(def mxGraphView js/mxGraphView)
(def mxStencil js/mxStencil)
(def mxStencilRegistry js/mxStencilRegistry)

(defn mx-load-xml-document [fpath]
  (-<> (fio/simple-slurp fpath)
       (.parseXml mxUtils <>)
       (aget "documentElement")))

;; the stylesheet is what enables rich shapes (ellipse, rhombus, etc)
(def default-stylesheet
  (try
    (mx-load-xml-document "resources/public/mxgraph/stylesheet.xml")
    (catch js/Object e
      nil)))

;; stencils enable specialized geometry
(doseq [stencil-filepath ["resources/public/mxgraph/flowchart_stencil.xml"]]
  (let [root (mx-load-xml-document stencil-filepath)
        mx-package-name (-> root
                            (.getAttribute "name")
                            (clojure.string/lower-case))]
    (doseq [shape-node (-> (aget root "children")
                           (array-seq))]
      (try
        (let [stencil-registry-name
              (->> (.getAttribute shape-node "name")
                   (clojure.string/lower-case)
                   (str mx-package-name "."))]
          (when (< 0 $DEBUG-LEVEL)
            (js/console.log "adding stencil: "
                            stencil-registry-name
                            shape-node))
          (.addStencil mxStencilRegistry
                       stencil-registry-name
                       (new mxStencil shape-node)))
        (catch js/Object e
          nil)))))

;; intention is to change appearance here. need to verify whether it does
(doto (aget mxGraphView "prototype")
  (aset "gridSteps" 4)
  (aset "minGridSize" 4)
  (aset "minGridSize" "#E0E0E0"))

(defn string-to-number [s]
  (js/parseFloat s))

(def number-like? (or
                   number?
                   #(let [n (string-to-number %)]
                      (= n n))))

(defrecord SpecCodec
    [spec in out])
(defn key-to-codec-mapper
  ([ks]
   (key-to-codec-mapper ks identity identity))
  ([ks in]
   (key-to-codec-mapper ks identity))
  ([ks in out]
   (->> ks
        (map (fn [k]
               [k
                (SpecCodec.
                 (spec/get-spec (keyword $this-namespace k))
                 in
                 out)]))
        (into {}))))

(spec/def ::_x number-like?)
(spec/def ::_y number-like?)
(spec/def ::_width number-like?)
(spec/def ::_height number-like?)
(def mx-numeric-keys
  (key-to-codec-mapper
   [:_x :_y :_width :_height]
   string-to-number
   str))

(spec/def ::_style string?)
(spec/def ::_value string?)
(def mx-string-keys
  (key-to-codec-mapper
   [:_style :_value]))

(spec/def ::_as #{"points" "geometry"
                  "sourcePoint" "targetPoint"})
(spec/def ::align
  #{(aget mxConstants "ALIGN_LEFT")
    (aget mxConstants "ALIGN_CENTER")
    (aget mxConstants "ALIGN_RIGHT")})
(spec/def ::verticalAlign
  #{(aget mxConstants "ALIGN_TOP")
    (aget mxConstants "ALIGN_MIDDLE")
    (aget mxConstants "ALIGN_BOTTOM")})
(def mx-set-keys
  (key-to-codec-mapper
   [:_as :align :verticalAlign]))

(def mx-truth-flag #{0 1 "0" "1"})
(spec/def ::_parent mx-truth-flag)
(spec/def ::_edge mx-truth-flag)
(spec/def ::_vertex mx-truth-flag)
(spec/def ::_relative mx-truth-flag)
(spec/def ::_connectable mx-truth-flag)
(spec/def ::html mx-truth-flag)
(spec/def ::dashed mx-truth-flag)
(spec/def ::rounded mx-truth-flag)
(spec/def ::resizable mx-truth-flag)
(def mx-boolean-keys
  (key-to-codec-mapper
   [:_parent
    :_edge
    :_vertex
    :_relative
    :_connectable
    :html
    :dashed
    :rounded
    :resizable]
   (fn [s]
     (case s
       (1 "1") true
       (0 "0") false))
   (fn [b]
     (case b
       true "1"
       false "0"))))

(def mx-speccodec-map
  (merge
   mx-numeric-keys
   mx-string-keys
   mx-set-keys
   mx-boolean-keys))

;; (->> (spec/registry)
;;      (filter (fn [[kw _]]
;;                (= $this-namespace (namespace kw))))
;;      (map (fn [[kw validator]]
;;             [kw validator])))

(spec/def ::mxNode
  (spec/keys :opt-un [::_x ::_y ::_width ::_height]))

(defn underscoreify-keys [m]
  (->> m
       (map (fn [[k v]]
              [(->> (name k)
                    (str "_")
                    (keyword))
               v]))
       (into {})))

(defn to-style-string [style-map]
  (->> style-map
       (map (fn [[attr val]]
              (cond-> attr
                (keyword? attr) name
                true (str ":" val ";"))))
       (clojure.string/join "")))

(defn graph-viz-node-to-mx-node [orig-map]
  {:pre [(spec/valid? ::grf/VizNode orig-map)]
   :post [(fn [out]
            (spec/valid? ::mxNode out))]}
  (let [converted (-> (apply dissoc orig-map mx-numeric-keys)
                      (dissoc :style)
                      (select-keys grf/base-node-keys)
                      (underscoreify-keys))]
    (-> (if-let [style-data (:style orig-map)]
          (assoc converted
                 :style
                 (cond-> style-data
                   (map? style-data) 
                   to-style-string))
          converted)
        (merge (->> (select-keys orig-map mx-numeric-keys)
                    (map (fn [[k v]]
                           [k (string-to-number v)]))
                    (into orig-map))))))

(defn mx-style->clj [mx-style-string]
  (->> (clojure.string/split
        mx-style-string
        #";")
       (map #(clojure.string/split % #"="))
       (map (fn [spl]
              (if (= 1 (count spl))
                [:_ (first spl)]
                (let [[k v] spl
                      kw (keyword k)
                      speccodec (mx-speccodec-map kw)]
                  [kw
                   ((if-let [reader (:in speccodec)]
                      reader
                      identity)
                    v)]))))
       (into {})))
  
(defn clj->mx-style [clj-style]
  (let [head (if-let [head (:_ clj-style)]
               (str head ";")
               nil)
        style-pair-map (dissoc clj-style :_)]
    (->> style-pair-map
         (map (fn [[k v]]
                (let [speccodec (mx-speccodec-map k)]
                  (str (name k) "="
                       ((if-let [writer (:out speccodec)]
                          writer
                          identity)
                        v)
                       ";"))))
         (apply str head))))

(defn get-cell-style-map [cell]
  (some-> (:_style cell)
          (mx-style->clj)))

(defn set-cell-style-map [cell style-map]
  (if-not style-map
    cell
    (let [style-string (clj->mx-style style-map)]
      (if (empty? style-string)
        cell
        (assoc cell :_style style-string)))))

(defn render-mxgraph-xml-to-element!
  ;; see mxgraph/javascript/examples/codec.html
  ;;     view-source:https://jgraph.github.io/mxgraph/javascript/examples/codec.html
  ;; (render-mxgraph-xml-to-element!
  ;;  (str "<mxGraphModel><root>"
  ;;       "<mxCell id=\"0\"/><mxCell id=\"1\" parent=\"0\"/><mxCell id=\"2\" vertex=\"1\" parent=\"1\" value=\"Interval 1\"><mxGeometry x=\"380\" y=\"0\" width=\"140\" height=\"30\" as=\"geometry\"/></mxCell><mxCell id=\"3\" vertex=\"1\" parent=\"1\" value=\"Interval 2\"><mxGeometry x=\"200\" y=\"80\" width=\"380\" height=\"30\" as=\"geometry\"/></mxCell><mxCell id=\"4\" vertex=\"1\" parent=\"1\" value=\"Interval 3\"><mxGeometry x=\"40\" y=\"140\" width=\"260\" height=\"30\" as=\"geometry\"/></mxCell><mxCell id=\"5\" vertex=\"1\" parent=\"1\" value=\"Interval 4\"><mxGeometry x=\"120\" y=\"200\" width=\"240\" height=\"30\" as=\"geometry\"/></mxCell><mxCell id=\"6\" vertex=\"1\" parent=\"1\" value=\"Interval 5\"><mxGeometry x=\"420\" y=\"260\" width=\"80\" height=\"30\" as=\"geometry\"/></mxCell><mxCell id=\"7\" edge=\"1\" source=\"2\" target=\"3\" parent=\"1\" value=\"Transfer1\"><mxGeometry as=\"geometry\"><Array as=\"points\"><Object x=\"420\" y=\"60\"/></Array></mxGeometry></mxCell><mxCell id=\"8\" edge=\"1\" source=\"2\" target=\"6\" parent=\"1\" value=\"\"><mxGeometry as=\"geometry\" relative=\"1\" y=\"-30\"><Array as=\"points\"><Object x=\"600\" y=\"60\"/></Array></mxGeometry></mxCell><mxCell id=\"9\" edge=\"1\" source=\"3\" target=\"4\" parent=\"1\" value=\"Transfer3\"><mxGeometry as=\"geometry\"><Array as=\"points\"><Object x=\"260\" y=\"120\"/></Array></mxGeometry></mxCell><mxCell id=\"10\" edge=\"1\" source=\"4\" target=\"5\" parent=\"1\" value=\"Transfer4\"><mxGeometry as=\"geometry\"><Array as=\"points\"><Object x=\"200\" y=\"180\"/></Array></mxGeometry></mxCell><mxCell id=\"11\" edge=\"1\" source=\"4\" target=\"6\" parent=\"1\" value=\"Transfer5\"><mxGeometry as=\"geometry\" relative=\"1\" y=\"-10\"><Array as=\"points\"><Object x=\"460\" y=\"155\"/></Array></mxGeometry></mxCell>"
  ;;       "</root></mxGraphModel>")
  ;;  (gdom/getElement "panel-A"))
  [xml-source target-el]
  (let [mx-container (clear-dom-element! target-el)
        xml-doc (.parseXml mxUtils xml-source)
        decoder (new mxCodec xml-doc)
        node (aget xml-doc "documentElement")
        graph (new mxGraph mx-container)]
    (.setHtmlLabels graph true)
    (when default-stylesheet
      (.setStylesheet graph
                      (-> (new mxCodec)
                          (.decode default-stylesheet))))
    ;; this call does the actual rendering to DOM
    (.decode decoder node (.getModel graph))
    graph))

(defn render-mxgraph-file-to-element!
  [xml-filepath target-el]
  (-> (fio/simple-slurp xml-filepath)
      (render-mxgraph-xml-to-element! target-el)))

(defn render-mxgraph-data-to-element!
  [mxgraph-data target-el]
  (-> mxgraph-data
      (clj->js)
      (js->xml)
      (render-mxgraph-xml-to-element! target-el)))

(defn get-model-from-mxgraph
  [mx-graph]
  (-> (new mxCodec)
      (js-invoke "encode" (.getModel mx-graph))))

(defn get-xml-from-mxgraph-model
  "this function takes an object that's like <mxGraphModel>...</mxGraphModel>"
  [mx-graph-model]
  (js-invoke mxUtils "getPrettyXml" mx-graph-model))

(defn get-xml-from-mxgraph
  [mx-graph]
  (-> mx-graph
      (get-model-from-mxgraph)
      (get-xml-from-mxgraph-model)))

(defn get-js-from-mxgraph
  [mx-graph]
  (-> mx-graph
      (get-xml-from-mxgraph)
      (xml->js)))

(defn get-clj-from-mxgraph
  [mx-graph-model]
  (-> mx-graph-model
      (get-js-from-mxgraph)
      (js->clj :keywordize-keys true)))

;; (defn transform-cells [mx-graph cell-transformer]
;;   (transform
;;    [spct/MAP-VALS
;;     spct/MAP-VALS
;;     spct/MAP-VALS
;;     spct/ALL
;;     ;; you can drill into `_vertex` only
;;     ;; or e.g.            `_edge`   only
;;     ;; #(:_vertex %)
;;     ]
;;    cell-transformer
;;    mx-graph))

(defn transform-cells-in-mxgraph [mx-graph cell-transformer]
  (transform
   [:mxGraphModel
    :root
    :mxCell
    spct/ALL]
   cell-transformer
   mx-graph))
