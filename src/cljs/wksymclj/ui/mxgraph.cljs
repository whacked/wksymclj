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
             :refer [xml->js js->xml]]))

;; mx dependencies
(def mxGraph js/mxGraph)
(def mxRubberband js/mxRubberband)
(def mxUtils js/mxUtils)
(def mxCodec js/mxCodec)

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
  (js-invoke mxUtils "getXml" mx-graph-model))

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
