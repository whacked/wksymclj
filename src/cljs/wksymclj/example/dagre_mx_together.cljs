(ns wksymclj.example.dagre-mx-together
  (:require [goog.dom :as gdom]
            [reagent.core :as r]
            [cljs.nodejs :as nodejs]
            
            [wksymclj.data-manipulation.graph
             :as grf
             :refer [get-edge-midpt
                     state-declaration-to-flow-graph]]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.ui.graph-codec :as graph-codec]

            [wksymclj.data-manipulation.xml
             :refer [xml->js js->xml]]
            [wksymclj.ui.mxgraph :as mx
             :refer [underscoreify-keys]]
            [com.rpl.specter :as spct])
  (:require-macros
   [swiss.arrows :refer [-<> -<>>]]
   [com.rpl.specter
    :refer [select transform]]))

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
          colors ["red" "blue" "green" "yellow" "pink"
                  "purple" "brown" "violet" "pumpkin"]
          mxgraph-data (graph-codec/dagre-graph-to-mxgraph-data dagre-graph)]
      
      ;; ;; render without modification
      ;; (mx/render-mxgraph-data-to-element!
      ;;  mxgraph-data
      ;;  $target-el)

      (-> mxgraph-data
          (mx/transform-cells-in-mxgraph
           (fn [cell]
             (->> cell
                  ((fn [cell]
                     (if-not (:mxGeometry cell)
                       cell
                       (if-let [{:keys [_width _height]}
                                (:mxGeometry cell)]
                         (-> cell
                             (update-in [:mxGeometry :_width]
                                        (partial * (+ 1 (rand))))
                             (update-in [:mxGeometry :_height]
                                        (partial * (+ 1 (rand)))))))))
                  ((fn [cell]
                     (->> cell
                          (mx/get-cell-style-map)
                          ((fn [style]
                             (-> style
                                 (assoc :fillColor
                                        (rand-nth colors))
                                 (assoc :strokeColor
                                        (rand-nth colors))
                                 (assoc :strokeWidth (inc (rand-int 10)))
                                 (assoc :dashed (rand-int 2))
                                 (dissoc :opacity (+ 0.3 (* 0.7 (rand))))

                                 (dissoc :fontFamily) ;; Verdana
                                 (dissoc :fontSize)
                                 (assoc :labelBackgroundColor
                                        (rand-nth colors)))))
                          (mx/set-cell-style-map cell)))))))
          (mx/render-mxgraph-data-to-element! $target-el))))

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
