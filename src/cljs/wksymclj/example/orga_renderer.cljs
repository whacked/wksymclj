(ns wksymclj.example.orga-renderer
  (:require [goog.dom :as gdom]
            [reagent.core :as r]
            [cljs.nodejs :as nodejs]
            [cljs.pprint]
            
            [wksymclj.nodejs-interface.fileio :as fio]
            [wksymclj.codec.org-mode :as wk-org]
            
            [com.rpl.specter :as spct]

            [wksymclj.ui.browser-interop
             :refer [clear-dom-element! set-element-style!]]
            [clojure.string]

            ;; C&P frorm dagre mx
            [goog.dom :as gdom]
            [reagent.core :as r]
            [cljs.nodejs :as nodejs]
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
            )


  (:require-macros
   [swiss.arrows :refer [-<> -<>>]]
   [com.rpl.specter
    :refer [select transform recursive-path]]))

(defn parse-drawer-text [drawer-text]
  (->> (clojure.string/split
        drawer-text
        #"\n")
       (map clojure.string/trim)
       (remove empty?)
       (map (fn [line]
              (if-let [matches (re-matches #":([^:]+):\s*(.+)" line)]
                [(keyword (get matches 1))
                 (get matches 2)])))
       (remove empty?)
       (into {})))

(defn get-first-value [ast-node]
  (let [value (aget ast-node "value")]
    (if-not (empty? value)
      value
      (if-let [children (some-> (aget ast-node "children")
                                (array-seq))]
        (loop [remain children
               out nil]
          (if (or (empty? children)
                  (not (empty? out)))
            out
            (recur (rest children)
                   (some-> (first children)
                           (get-first-value)))))))))

(defn postprocess-ast
  "ast nodes contain
  - type
  - children
  - name
  - value
  - parent (circular).
  we will manually build the clj structure sans the parent"
  [ast-node]
  (let [node-type (aget ast-node "type")
        node-name (aget ast-node "name")
        node-value (aget ast-node "value")
        node-children (aget ast-node "children")]
    (merge
     {:type node-type
      :name node-name
      :value (case node-type
               "headline"
               (str
                "* "
                (get-first-value ast-node))

               "drawer"
               (str (parse-drawer-text node-value))
                
               (str "[" node-type "] &lt;h1&gt;think some keywords&lt;/h1&gt;"
                    node-value))}
     (case node-type
       "headline"
       nil
         
       {:children (->> node-children
                       (array-seq)
                       (map postprocess-ast))}))))

(defn extract-printable
  ;; see wksymclj.codec.org-mode/org->clj-ast
  ([clj-ast]
   (extract-printable clj-ast [] {:node-map {}
                                  :edge-seq []}))
  ([clj-ast
    search-path
    out-map]
   
   (comment
     (let [my-org-ast (-> org-content
                          (wksymclj.codec.org-mode/orga-parse)
                          (postprocess-ast))]
       (->> my-org-ast
            (extract-printable)
            (:edge-seq)
            (take 4))))
   
   (let [children (:children clj-ast)]
     (loop [child-index 0
            child-remain children
            node-map (assoc (:node-map out-map)
                            search-path {:label (:value clj-ast)
                                         :token (:name clj-ast)
                                         :type (:type clj-ast)})
            edge-seq (:edge-seq out-map)]
       (if (empty? child-remain)
         {:node-map node-map
          :edge-seq edge-seq}
         (let [child (first child-remain)
               this-location (conj search-path :children child-index)
               next-out (extract-printable
                         child
                         this-location
                         {:node-map node-map
                          :edge-seq (conj edge-seq
                                          [search-path this-location])})]
           (recur (inc child-index)
                  (rest child-remain)
                  (:node-map next-out)
                  (:edge-seq next-out))))))))

(comment
  ;; sample setup
  
  (def my-flow-graph
    {:node-list
     (->> my-printable
          (:node-map)
          (map (fn [[name node]]
                 {:label (:label node)
                  :name (str name)
                  :width 50 :height 30})))
     :edge-list
     (->> my-printable
          (:edge-seq)
          (map (fn [[src tgt]]
                 [(str src)
                  (str tgt)
                  {:label ""}]))
          (vec))})
  
  (def my-mxgraph
    (let [$target-el (gdom/getElement "panel-A")
          dagre-graph (dagre/make-dagre
                       (:node-list my-flow-graph)
                       (:edge-list my-flow-graph))
          mxgraph-data (graph-codec/dagre-graph-to-mxgraph-data dagre-graph)

          container (doto
                        (gdom/createElement "div")
                      (set-element-style! {:width "100%"
                                           :height "100%"
                                           :overflow "scroll"}))]

      (doto $target-el
        (clear-dom-element!)
        (.appendChild container))
      ;; ;; render without modification
      ;; (mx/render-mxgraph-data-to-element!
      ;;  mxgraph-data
      ;;  container)

      (-> mxgraph-data
          
          ;; postprocess
          (mx/transform-cells-in-mxgraph
           (fn [cell]
             (->> cell
                  ((fn [cell]
                     (-<>> cell
                           (mx/get-cell-style-map)
                           ((fn [style]
                              (-> style
                                  (assoc :whiteSpace "wrap")
                                  (assoc :html true)
                                  (assoc :dashed false)
                                  (assoc :strokeWidth 1))))
                           (mx/set-cell-style-map cell)
                           ))))))
          (clj->js)
          (js->xml)
          (clojure.string/replace #"&amp;" "&")
          (js/console.log)
          ;; (mx/render-mxgraph-xml-to-element! $target-el)
          )
      ))

  (-> my-mxgraph
      (mx/get-xml-from-mxgraph)
      (js/console.log))

  (mx/render-mxgraph-file-to-element!
   "./temp.xml"
   (gdom/getElement "panel-A"))
  (js/console.log my-mxgraph)
  

  (let [target-el (gdom/getElement "panel-A")
        my-org "* foobar\n\nhello\n\n** somethjing else\n\nboalh"]
    (r/render
     (wk-org/orga-org->hiccup my-org)
     target-el)))
