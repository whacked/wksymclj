(ns wksymclj.example.dagre-mx-together
  (:require [goog.dom :as gdom]
            [reagent.core :as r]
            [wksymclj.nodejs-interface.fileio :as fio]
            
            [wksymclj.data-manipulation.graph
             :as grf
             :refer [get-edge-midpt
                     state-declaration-to-flow-graph]]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.codec.graph :as graph-codec]

            [wksymclj.data-manipulation.xml
             :refer [xml->js js->xml]]
            [wksymclj.ui.mxgraph :as mx
             :refer [underscoreify-keys]]
            [com.rpl.specter :as spct]
            [cljs.pprint])
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
                  "purple" "brown" "violet" "orange"]
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
                                 (assoc :dashed (rand-nth [true false]))
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

(comment
  (def node-attribute-db
    {"need-library"
     {:shape :Terminator
      :section "browser"
      :color "pink"}
     "think-keywords"
     {:shape :Process
      :section "browser"
      :color "orange"}
     "search-engine"
     {:shape :Decision
      :section "browser"
      :color "yellow"}
     "found-match"
     {:shape :Data
      :section "browser"
      :color "lime"}
     "no-match"
     {:shape :Step
      :section "browser"
      :color "skyblue"
      :x 250
      :y 320}
     "download-library"
     {:shape :Process
      :section "terminal"
      :color "salmon"}
     "try-out"
     {:shape :Terminator
      :section "terminal"
      :color "violet"}})

  (defn get-node-attribute-map [node-name]
    (get node-attribute-db node-name))
  
  ;; draw swimlanes
  (let [lane-mapping {"browser" {:x 40
                                 :color "purple"}
                      "terminal" {:x 320
                                  :color "brown"}}
        _node-width 120
        _node-height 80]
    
    (def my-flow-graph
      (let [w _node-width
            h _node-height
            _node (fn [_name _label]
                    (let [node-attr (get-node-attribute-map _name)]
                      (merge
                       {:name _name
                        :label _label
                        :width w
                        :height (if (= (:shape node-attr) :Process)
                                  w h)}
                       (dissoc node-attr
                               :x :y))))]
        {:node-list
         [(_node "need-library"
                 "Need a <b>graph drawing library</b>")
          (_node "think-keywords"
                 "<i>think</i> some keywords")
          (_node "search-engine"
                 "Search on <u>search engine</u>")
          (_node "found-match"
                 "<h3>nice loooking match</h3>")
          (_node  "no-match"
                  "nothing promising")
          (_node "download-library"
                 "download library")
          (_node "try-out"
                 "try it out")]
         :edge-list
         [["need-library" "think-keywords"]
          ["think-keywords" "search-engine"]
          ["search-engine"
           "found-match"
           {:label "YES"}]
          ["search-engine"
           "no-match"
           {:label "NO"}]
          ["no-match" "think-keywords"]
          ["found-match" "download-library"]
          ["download-library" "try-out"]]}))
    
    (def my-mxgraph
      (let [$target-el (gdom/getElement "panel-A")
            dagre-graph (dagre/make-dagre
                         (:node-list my-flow-graph)
                         (:edge-list my-flow-graph))
            
            colors ["red" "blue" "green" "yellow" "pink"
                    "purple" "brown" "violet" "pumpkin"]
            
            _node-mapping (->> (:node-list my-flow-graph)
                               (mapv (fn [node]
                                       [(:name node) node]))
                               (into {}))
            dagre-nodes (-> dagre-graph
                            (wksymclj.ui.dagre/get-dagre-node-seq))
            _mx-mapping (->> (wksymclj.ui.dagre/get-node-id-mapping
                              dagre-nodes 2)
                             (map (fn [[node-name mx-id]]
                                    [mx-id (_node-mapping node-name)]))
                             (into {}))
            mxgraph-data (graph-codec/dagre-graph-to-mxgraph-data dagre-graph)]
        
        ;; render without modification
        ;; (mx/render-mxgraph-data-to-element!
        ;;  mxgraph-data
        ;;  $target-el)

        (-> mxgraph-data
            
            ;; postprocess
            (mx/transform-cells-in-mxgraph
             (fn [cell]
               (->> cell
                    ((fn [cell]
                       (let [node-spec (_mx-mapping (:_id cell))
                             node-name (:name node-spec)
                             node-attr (get-node-attribute-map node-name)]
                         (cond node-spec
                               (let [section (:section node-spec)
                                     section-x (get-in lane-mapping [section :x]
                                                       0)
                                     section-color (get-in lane-mapping [section :color]
                                                           "#9673a6")
                                     cell-color (:color node-spec)]
                                 (-<>> cell
                                       
                                       (mx/get-cell-style-map)
                                       ((fn [style]
                                          (-> style
                                              
                                              (assoc :whiteSpace "wrap")
                                              (assoc :html true)
                                              (assoc :fillColor cell-color)
                                              (assoc :strokeColor section-color)
                                              (assoc :dashed false)
                                              (assoc :strokeWidth 3)

                                              (merge

                                               (case (:shape node-spec)
                                                 :Process {:_ "ellipse"}
                                                 
                                                 :Decision {:_ "rhombus"}

                                                 :Terminator {:shape "mxgraph.flowchart.terminator"}
                                                 
                                                 :Data {:shape "mxgraph.flowchart.document"}
                                                 
                                                 :Step {:rounded true}

                                                 nil))
                                              )))
                                       (mx/set-cell-style-map cell)
                                       (update-in <> [:mxGeometry :_x]
                                                  (fn [cur-x]
                                                    ;; putting all nodes into the same swimlane x
                                                    ;; will cause overlaps, so use dagre's positioning
                                                    ;; first, and gravitate along lane
                                                    (-> cur-x
                                                        (+ section-x)
                                                        (/ 2)
                                                        (+ (/ section-x 2)))))
                                       (update-in <> [:mxGeometry]
                                                  (fn [mxg]
                                                    (merge mxg
                                                           (-> (select-keys node-attr [:x :y])
                                                               (underscoreify-keys)))))))

                               (cell :_edge)
                               (update-in cell [:mxGeometry]
                                          (fn [mxg]
                                            (let [source-section (-> (:_source cell)
                                                                     _mx-mapping
                                                                     :section)
                                                  target-section (-> (:_target cell)
                                                                     _mx-mapping
                                                                     :section)
                                                  source-lane-x (or (get-in lane-mapping
                                                                            [source-section :x]))
                                                  target-lane-x (or (get-in lane-mapping
                                                                            [target-section :x]))
                                                  ]
                                              ;; adjust waypoint?
                                              ;; this is necessary because the renderer will take
                                              ;; take coordinates from the original cell, not the
                                              ;; one after it has been nudged into the swimlane.
                                              ;; (assoc-in mxg
                                              ;;           [:Array :mxPoint :_x]
                                              ;;           (+ (/ (+ source-lane-x
                                              ;;                    target-lane-x)
                                              ;;                 2)
                                              ;;              (/ _node-width 2)))
                                              ;; Or just remove the waypoint?
                                              (dissoc mxg :Array))))

                               :else cell)))))))
            ((fn [x]
               (cljs.pprint/pprint x)
               x))
            (mx/render-mxgraph-data-to-element! $target-el))))

    ;; the codec based renderer does not seem to understand the shapes,
    ;; but C&P into draw.io does work. use that for now :-(
    (->> (mx/get-xml-from-mxgraph my-mxgraph)
         (fio/simple-spit "temp.xml"))))
