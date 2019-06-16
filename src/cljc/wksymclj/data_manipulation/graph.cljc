(ns wksymclj.data-manipulation.graph
  (:require [wksymclj.data-manipulation.collection
             :as wk-coll]
            [cljs.spec.alpha :as spec]
            [schema.core :as scm
             #?@(:cljs [:include-macros true])]))

(spec/def ::x number?)
(spec/def ::y number?)
(spec/def ::width number?)
(spec/def ::height number?)
(spec/def ::source #(or (number? %) (string? %)))
(spec/def ::target #(or (number? %) (string? %)))

(spec/def ::VizNode
  (spec/keys :opt-un [::x ::y ::width ::height]))

(spec/def ::VizEdge
  (spec/keys :req-un [::source ::target]))

;; flow-graph structure
(spec/def ::FlowgraphNode
  (spec/keys :opt-un [::x ::y ::width ::height]))
(spec/def ::FlowgraphEdge
  (spec/or :source-target (spec/tuple string? string?)
           :source-target-attr (spec/tuple string? string? map?)))

(def example-flow-graph
  {:node-list
   [{:name "a"}
    {:name "b"}
    {:name "c"}]
   :edge-list
   [["a" "b"]
    ["a" "c"]]})

(def base-node-keys
  (->> (spec/describe ::VizNode)
       (drop-while (fn [item]
                     (not= item 'keys)))
       (rest)
       (filter sequential?)
       (apply concat)))

(defn get-edge-midpt [pt-seq]
  (let [npt (count pt-seq)
        midn (quot npt 2)]
    (if (odd? npt)
      (nth pt-seq midn)
      (let [p1 (nth pt-seq midn)
            p2 (nth pt-seq (dec midn))]
        {:x (/ (+ (:x p1) (:x p2)) 2)
         :y (/ (+ (:y p1) (:y p2)) 2)}))))


(def StateMatchConditionToken scm/Any)
(def StateMatchEdgeLabel scm/Str)
(def StateMatchStepId scm/Str)
(def StateTransition
  [StateMatchConditionToken
   StateMatchEdgeLabel
   StateMatchStepId])

(def FlowGraphNodeUniqueId scm/Str)
(def FlowGraphNode
  {:name FlowGraphNodeUniqueId
   (scm/optional-key :label) scm/Str ;; override displayed name at render time
   (scm/optional-key :width) scm/Any
   (scm/optional-key :height) scm/Any
   ;; user defined
   (scm/optional-key :type) scm/Any ;; example of common optional key
   scm/Keyword scm/Any})

(def FlowGraphEdgeProperties {scm/Keyword scm/Any})
(def FlowGraphEdge
  [FlowGraphNodeUniqueId
   FlowGraphNodeUniqueId
   FlowGraphEdgeProperties])

(defn state-declaration-to-flow-graph
  "
  `state-declaration-list` is a list of 6-entry seqs, like:
    :step \"01c-2-branch-right\"                 ;; id
    \"try new branch?\"                          ;; label
    [[true \"cool\" \"some-aggregation-step\"]   ;; transitions: [match-condition edge-label next-step-id]
     [false \"fail\" \"02-fallback-procedure\"]]
    {}                                           ;; other
    validator-function                           ;; returns the match-condition
  "
  [state-declaration-list]
  {:node-list
   (->> state-declaration-list
        (map (fn [[type name label state-list & _]]
               ;; this looks redundant re: node creation:
               ;; it creates a node per state!
               (for [state state-list]
                 {:label label
                  :name name
                  :width 144
                  :height 20
                  :type type})))
        (apply concat))

   :edge-list
   (->> state-declaration-list
        (#(map
           vector
           (drop-last %)
           (drop 1 %)))
        (#(for [[pre post] %]
            (let [[pr-type pr-name pr-label pr-state-list & _] pre
                  [po-type po-name po-label po-state-list & _] post]
              (for [pr-state pr-state-list
                    po-state po-state-list]
                (let [po-specified (if (= 3 (count pr-state))
                                     (last pr-state)
                                     po-name)]
                  [pr-name po-specified {:label
                                         (str
                                          (first pr-state)
                                          "(" (second pr-state) ")")}])))))
        (apply concat))})

(defn get-next-state-list [transition-spec now-state now-world]
  (->> transition-spec
       (map (fn [[name next world]]
              (if (and (= now-state name)
                       (wk-coll/is-submap? world now-world))
                next)))
       (filter identity)))

(defn derive-pr->po-map
  "edge of [pr-name po-name & attrs]
  returns pr-name -> [po-name ...]"
  [edges]
  (->> edges
       (map (fn [[pr-name po-name & _]]
              {pr-name [po-name]}))
       (apply merge-with concat)))


(defn count-incoming-edges
  "also want nodes to account for orphans"
  [edges & [nodes]]
  (let [pr-po-map (derive-pr->po-map edges)]
    (merge
     ;; create defaults to account for all
     ;; nodes, including roots and orphans
     (->> (concat
           (map (fn [node]
                  [(:name node) 0])
                nodes)
           (map (fn [[pr-name & _]]
                  [pr-name 0])
                edges))
          (into {}))
     ;; get incoming edge counts for
     ;; connected nodes
     (->> pr-po-map
          (vals)
          (apply concat)
          (frequencies)))))

(defn find-root-nodes [edges & [nodes]]
  (->> (count-incoming-edges edges nodes)
       (filter (fn [[k v]]
                 (= 0 v)))
       (map first)))

(defn get-node-walk-to-leaf-coll
  ([edges start-node-name]
   (get-node-walk-to-leaf-coll
    (derive-pr->po-map edges)
    start-node-name
    #{start-node-name}
    [start-node-name]))
  ([pr-po-map start-node-name visited walk-history]
   (if-let [next-nodes (pr-po-map start-node-name)]
     (->> next-nodes
          (map (fn [next-node-name]
                 (get-node-walk-to-leaf-coll
                  pr-po-map
                  next-node-name
                  (conj visited next-node-name)
                  (conj walk-history next-node-name))))
          ;; ref the flatten algo; see
          ;; https://clojuredocs.org/clojure.core/tree-seq; we
          ;; want to find terminal entries in the nested seqs of
          ;; `walk-history`s and extract the single-layer seqs,
          ;; (identified by having `string?` as first item),
          ;; which contain the unique walks
          (tree-seq sequential? seq)
          (rest)
          (filter
           (fn [item]
             (and (sequential? item)
                  (->> (first item)
                       (string?))))))
     walk-history)))

(defn get-node-height
  "number of edges to the leaf starting from a given node"
  [edges node-name]
  (->> (get-node-walk-to-leaf-coll edges node-name)
       (map count)
       (apply max)))

(defn get-bandwidth
  "length of longest edge (walk) from a root to a leaf"
  [edges]
  (let [root-nodes (find-root-nodes edges)]
    (->> root-nodes
         (map (partial get-node-height edges))
         (apply max))))
