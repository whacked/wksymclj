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

            [wksymclj.ui.browser-interop :as browser]))

;; there is a glob.sync method, but let's try to
;; concurrent-ize .org and .tid loaders.
;; sync  (.sync glob path)
;; async (glob path callback)
(def glob (nodejs/require "glob"))

(def file-db (atom {}))

(defn load-directory! [base-dir db-atom]
  (doseq [extension ["org" "tid"]]
    (glob (fio/path-join base-dir (str "**/*." extension))
          (fn [err matches]
            (when err
              (throw err))
            (->> matches
                 (array-seq)
                 (mapv (fn [path]
                         (let [file-name (clojure.string/replace
                                          path (re-pattern
                                                (str "^" base-dir "/"))
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
                 (conj node-list {:name k
                                  :label (:title md)})
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
          $target-el (gdom/getElement "panel-A")]
      (doto $target-el
        (browser/set-element-style!
         {:overflow "scroll"
          :border "2px solid red"}))
      
      (-> (dagre/make-dagre
           (:node-list my-flow-graph)
           (:edge-list my-flow-graph))
          (graph-codec/dagre-graph-to-mxgraph-data)
          (mx/render-mxgraph-data-to-element! $target-el)))))
