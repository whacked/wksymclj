(ns wksymclj.data-manipulation.graph
  (:require [wksymclj.data-manipulation.collection
             :as wk-coll]
            [cljs.spec.alpha :as spec]))

(spec/def ::x number?)
(spec/def ::y number?)
(spec/def ::width number?)
(spec/def ::height number?)

(spec/def ::VizNode
  (spec/keys :opt-un [::x ::y ::width ::height]))

(def base-node-keys
  (->> (spec/describe ::VizNode)
       (drop-while (fn [item]
                     (not= item 'keys)))
       (rest)
       (filter sequential?)
       (apply concat)))

;; placeholder: no clear generalization path now
;; (spec/def ::GraphVizEdge)

(defn get-edge-midpt [pt-seq]
  (let [npt (count pt-seq)
        midn (quot npt 2)]
    (if (odd? npt)
      (nth pt-seq midn)
      (let [p1 (nth pt-seq midn)
            p2 (nth pt-seq (dec midn))]
        {:x (/ (+ (:x p1) (:x p2)) 2)
         :y (/ (+ (:y p1) (:y p2)) 2)}))))

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
