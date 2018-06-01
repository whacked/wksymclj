(ns wksymclj.example.dagre-mx-together
  (:require [goog.dom :as gdom]
            [reagent.core :as r]
            [cljs.nodejs :as nodejs]
            
            [wksymclj.data-manipulation.graph
             :as grf
             :refer [get-edge-midpt
                     state-declaration-to-flow-graph]]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.ui.graph-codec :as graph-codec
             :refer [underscoreify-keys]]

            [wksymclj.data-manipulation.xml
             :refer [xml->js js->xml]]
            [wksymclj.ui.mxgraph :as mx]
            ))

;; specify graph data
;; - with node size information
;; - with no position information
;; position it with dagre
;; draw it with mxgraph

(defn dagre->mx-geometry [dagre-node]
  (-> dagre-node
      (select-keys [:x :y :width :height])
      (underscoreify-keys)))

(comment
  (def my-flow-graph
    (let [w 144
          h 20
          _node (fn [_label _name]
                  {:label _label
                   :name _name
                   :width w :height h})
          
          _step (fn [_label _name]
                  (-> (_node _label _name)
                      (assoc :type :step)))

          _choice (fn [_label _name]
                    (-> (_node _label _name)
                        (assoc :type :choice)))
          ]
      {:node-list
       [(_step   "Start!"          "00-home-screen")
        (_choice "branch point 1"  "01c-make-a-choice-1")
        (_choice "branch point 1"  "01c-make-a-choice-1")
        (_step   "LEFT SIDE"       "01c-1-branch-left")
        (_step   "try new branch?" "01c-2-branch-right")
        (_step   "try new branch?" "01c-2-branch-right")
        (_step   "some fall back"  "02-fallback-procedure")
        (_step   "aggregate"       "some-aggregation-step")
        (_step   "all done"        "04-end-state")]
       :edge-list
       [["00-home-screen"        "01c-make-a-choice-1"   {:label "true(END)"}]
        ["00-home-screen"        "01c-make-a-choice-1"   {:label "true(END)"}]
        ["01c-make-a-choice-1"   "01c-1-branch-left"     {:label "true(YES)"}]
        ["01c-make-a-choice-1"   "01c-2-branch-right"    {:label "false(NO)"}]
        ["01c-1-branch-left"     "some-aggregation-step" {:label "true(DONE)"}]
        ["01c-1-branch-left"     "some-aggregation-step" {:label "true(DONE)"}]
        ["01c-2-branch-right"    "some-aggregation-step" {:label "true(cool)"}]
        ["01c-2-branch-right"    "02-fallback-procedure" {:label "false(fail)"}]
        ["02-fallback-procedure" "some-aggregation-step" {:label "true(END)"}]
        ["some-aggregation-step" "04-end-state"          {:label "true(END)"}]]}))

  (def my-mxgraph
    (let [$target-el (gdom/getElement "panel-A")

          dagre-graph (dagre/make-dagre
                       (:node-list my-flow-graph)
                       (:edge-list my-flow-graph))
          
          node-seq (dagre/get-dagre-node-seq dagre-graph)
          edge-seq (dagre/get-dagre-edge-seq dagre-graph)]
      
      (mx/render-mxgraph-data-to-element!
       (graph-codec/dagre-graph-to-mxgraph-data dagre-graph)
       $target-el)))

  (def my-mxgraph-clj-initial
    (mx/get-clj-from-mxgraph my-mxgraph))

  ;; move the nodes/edges around and save state
  (def my-mxgraph-clj-updated
    (mx/get-clj-from-mxgraph my-mxgraph))

  ;; restore the original
  (mx/render-mxgraph-data-to-element!
   my-mxgraph-clj-initial
   (gdom/getElement "panel-A"))

  ;; restore the updated
  (mx/render-mxgraph-data-to-element!
   my-mxgraph-clj-updated
   (gdom/getElement "panel-A")))
