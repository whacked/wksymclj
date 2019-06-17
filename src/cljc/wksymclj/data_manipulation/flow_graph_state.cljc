(ns wksymclj.data-manipulation.flow-graph-state
  (:require [schema.core :as scm
             :include-macros true]
            [wksymclj.data-manipulation.graph :as grf]))

(def TransitionDescription
  [(scm/one scm/Any "match")          ;; transition match state
   (scm/one scm/Str "label")          ;; transition label / name
   (scm/optional scm/Str "post-step") ;; explicit post-step
   ])

(def StateDeclaration
  ;; (:step
  ;;  "00-home-screen"
  ;;  "Start!"
  ;;  [[true "END"]]
  ;;  {}
  ;;  #object
  ;;  [wksymclj$state_control$validation$NOP_validator]
  ;;  )
  [(scm/one (scm/enum :step :choice) "node-type") ;; type of step
   (scm/one scm/Str "internal-name")
   (scm/one scm/Str "ui-label")
   (scm/one [TransitionDescription] "transition-description")
   (scm/one {scm/Any scm/Any} "data-to-collect")
   (scm/one
    scm/Any "validators" ;; should be a step (validator) function
    )])

(defn state-declaration-list->flow-graph
  [state-declaration-list]
  {:pre [(scm/validate [StateDeclaration] state-declaration-list)]}
  {:node-list
   (->> state-declaration-list
        (map (fn [[type name label state-list node-data & _]]
               (merge
                {:label label
                 :name name
                 :width 144
                 :height 30
                 :type type}
                node-data))))

   :edge-list
   (->> state-declaration-list
        (mapv
         (fn [[pr-type pr-name pr-label pr-state-list & _]]
           (->> pr-state-list
                (mapv (fn [pr-state]
                        (when (< 2 (count pr-state))
                          (let [po-specified (nth pr-state 2)
                                attrs (if (< 3 (count pr-state))
                                        (nth pr-state 3))]
                            [pr-name
                             po-specified
                             (merge
                              {:label
                               (str
                                (first pr-state)
                                "(" (second pr-state) ")")}
                              attrs)])))))))
        (apply concat)
        (remove empty?))})


(def NodeTraverseState
  {(scm/optional-key :type) (scm/enum :state-data)
   (scm/optional-key :label) scm/Str
   (scm/optional-key :outdegree) scm/Int
   scm/Keyword scm/Any})

(def EdgeTraverseState
  {(scm/required-key :type) (scm/enum :graph-edge)
   (scm/optional-key :label) scm/Str
   scm/Keyword scm/Any})

(def NodeIdOrEdgeSourceTarget
  (scm/conditional
   sequential? [(scm/one scm/Str "source id")
                (scm/one scm/Str "target id")]
   :else scm/Str))

(def GraphTraverseState
  {NodeIdOrEdgeSourceTarget
   (scm/conditional
    (fn [datum]
      (= (:type datum) :state-data))
     NodeTraverseState
     :else EdgeTraverseState)})

(defn state-declaration-list->graph-state [state-declaration-list]
  {:post [(scm/validate GraphTraverseState %)]}
  (into {}
        (concat

         ;; expand state-defined extra data
         (->> state-declaration-list
              (map
               (fn [[_ state-name label state-list collect _]]
                 ;; state-name-qualified attributes
                 (conj
                  ;; collect extra attributes
                  ;; from the "collect" field
                  (->>
                   (keys collect)
                   (map (fn [attr]
                          (let [attr-string (if (keyword? attr)
                                              (name attr)
                                              attr)]
                            [
                             ;; (str state-name "/" attr-string)

                             ;; [state-name attr-string]
                             
                             (keyword state-name attr-string)

                             {:type :state-data
                              :label attr-string
                              :outdegree 1}]))))
                  ;; the node results in its own (single)
                  ;; exit state, so add it to the key list
                  [state-name
                   {:type :state-data
                    :label label
                    :outdegree (count state-list)}])))
              (apply concat)
              (into {}))

         ;; redundant; clean this up
         ;; edge state map
         (->> state-declaration-list
              ;; create pairs of consecutive state declarations
              (#(map vector
                     (drop-last %)
                     (drop 1 %)))
              (map
               (fn [[pre post]]
                 (let [[pr-type pr-name pr-label pr-state-list & _] pre
                       [po-type po-name po-label po-state-list & _] post]
                   (->> pr-state-list
                        (map (fn [pr-state]
                               (let [
                                     ;; determine whether we should select a custom
                                     ;; post-node, or auto-select one
                                     po-specified (if (= 3 (count pr-state))
                                                    (last pr-state)
                                                    po-name)
                                     ]
                                 [[pr-name po-specified]
                                  {:value (first pr-state)
                                   :type :graph-edge
                                   :label (str
                                           pr-name "---" po-specified
                                           ": "
                                           (first pr-state))}]
                                 )
                               ))
                        )
                   )
                 ))
              (apply concat)))))

(defn get-tagged-walk-steps [nodes edges]
  (let [pr-po-map (grf/derive-pr->po-map edges)
        root-nodes (grf/find-root-nodes edges)
        longest-edge-length (grf/get-bandwidth edges)
        node-lookup (->> nodes
                         (map (fn [node]
                                [(:name node) node]))
                         (into {}))]
    (->> root-nodes
         (map (partial grf/get-node-walk-to-leaf-coll edges))
         (apply concat)
         (map (fn [walk-path]
                (->> walk-path
                     (map-indexed
                      (fn [i node-name]
                        (let [node-data (node-lookup node-name)]
                          (assoc
                           (merge
                            (if (and (= 0 i)
                                     (not (get node-data :group)))
                              {:group :start})
                            node-data)
                           :_walk-step i))))))))))
