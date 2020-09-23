(ns wksymclj.example.dagre-state-view
  (:require [goog.dom :as gdom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            
            [wksymclj.state-control.validation :as vald]
            [wksymclj.data-manipulation.graph
             :as grf
             :refer [get-edge-midpt
                     state-declaration-to-flow-graph]]
            [wksymclj.ui.dagre :as dagre]
            [wksymclj.ui.browser-interop
             :refer [get-evt-value]]))

;; demo state declaration list
(def state-declaration-list
  (partition 6
             ;; - type of step
             ;; - internal name
             ;; - UI label
             ;; - transition description list
             ;;     - if explicit post-state not given,
             ;;       current state is automatically wired to next state,
             ;;       i.e. on completion, move to next adjacent state in this list
             ;; - what data does this step collect
             ;; - step validator;
             ;;   when triggered, it will evaluate the current master state,
             ;;   and:
             ;;     - if no condition is met, fire nil
             ;;     - if a condition is met within `transition description list`,
             ;;       fire that transition state
             [
              :step "00-home-screen"
              "Start!" [[true "END"]]
              {}
              vald/NOP-validator

              :choice "01c-make-a-choice-1"
              "branch point 1" [[true "YES" "01c-1-branch-left"]
                                [false "NO" "01c-2-branch-right"]]
              {}
              vald/NOP-validator

              :step "01c-1-branch-left"
              "LEFT SIDE" [[true "DONE" "some-aggregation-step"]]
              {}
              vald/all-non-nil

              :step "01c-2-branch-right"
              "try new branch?" [[true "cool" "some-aggregation-step"]
                                 [false "fail" "02-fallback-procedure"]]
              {} vald/NOP-validator
                                        
              :step "02-fallback-procedure"
              "some fall back" [[true "END"]]
              {}
              (fn [m]
                (doseq [[k v] m]
                  (js/console.log (str "CHECKING: " "%c" k)
                                  "color:gold;background:green;"))
                (every?
                 #(not (nil? %))
                 (vals m)))

              :step "some-aggregation-step"
              "aggregate" [[true "END"]]
              {}
              vald/all-non-nil

              :step "04-end-state"
              "all done" [[true "END"]]
              {}
              vald/all-non-nil
              ]))

(def flow-graph (grf/state-declaration-to-flow-graph state-declaration-list))

(def flow-dagre (dagre/make-dagre (:node-list flow-graph) (:edge-list flow-graph)))

(def graph-state (atom
                  {:node-seq (dagre/get-dagre-node-seq flow-dagre)
                   :edge-seq (dagre/get-dagre-edge-seq flow-dagre)
                   :traversal-status
                   ;; expand total graph visitable steps+paths (nodes+edges)
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
                                               :nstate 1}]))))
                                   ;; the node results in its own (single)
                                   ;; exit state, so add it to the key list
                                   [
                                    ;; [state-name]
                                    (keyword state-name)
                                    {:type :state-data
                                     :label label
                                     :nstate (count state-list)}]
                                   )
                                  )
                                )
                               (apply concat)
                               (into {}))

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
                                                   {:nstate 1
                                                    :value (first pr-state)
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
                               (apply concat)
                               )
                          ))
                   }))

(def app-state (r/atom {:text "asdf!"
                        :graph-history-display {:min 0
                                                :max nil
                                                :index nil}}))

(def state-history
  (r/atom []))

(defn save-current-state-to-undo! []
  (swap! state-history
         conj {:app-state @app-state
               :graph-state @graph-state})
  ;; (let [cur-max-index (dec (count @state-history))]
  ;;  (swap! app-state update :graph-history-display
  ;;         (fn [cur]
  ;;           (assoc cur
  ;;                  :end cur-max-index
  ;;                  :index cur-max-index))))
  )

(defn undo! []
  (when-let [prev-world (last @state-history)]
    (reset! app-state (:app-state prev-world))
    (reset! graph-state (:graph-state prev-world))
    (swap! state-history pop)))

(defn push-graph-step! [step]
  (let [traversal-status (:traversal-status @graph-state)]
    (swap! graph-state
           assoc
           :traversal-status
           (reduce
            (fn [m k]
              (update-in m [k]
                         #(assoc % :state nil)))
            traversal-status
            (keys traversal-status))
           :current-step step)))

(defn goto-step! [step-name]
  (save-current-state-to-undo!)
  (push-graph-step! step-name)
  (swap! app-state assoc :current-step step-name))

;; FIXME -- should not use dagre edges. should specify transition states?
;; or fix the transition state validators in flow/state-declaration-list
(def my-transition-spec
  (->>
   (js->clj (.edges flow-dagre) :keywordize-keys true)
   (map #(let [{:keys [w v]} %]
           [v w {}]))))

(defn go-next-step! []
  (let [now-step (or (:current-step @graph-state)
                     (:name
                      (nth (:node-seq @graph-state)
                           0)))
        next-step (or (if-let [next-candidate-list (seq
                                                    (grf/get-next-state-list my-transition-spec
                                                                             now-step
                                                                             {}))]
                        (rand-nth next-candidate-list))
                      (first (first my-transition-spec)))]
    (let [ ;; edge (first (:edge-seq @graph-state))
          edge-pair [now-step next-step]
          traversal-status (:traversal-status @graph-state)
          ]
      (if (= next-step (first (first my-transition-spec)))
        ;; started over / start from beginning
        (do
          (reset! state-history [])
          (push-graph-step! next-step))
        ;; load next step = ensure the traversed path gets status marked
        (do
          (save-current-state-to-undo!)
          (reset! graph-state
                  (assoc
                   (assoc-in
                    @graph-state
                    [:traversal-status edge-pair :state]
                    (:value (traversal-status edge-pair)))
                   :current-step next-step))))
      (swap! app-state assoc
             :current-step next-step
             :graph-history-display {:min 0
                                     :max nil
                                     :index nil})
      ;; (println [edge-pair
      ;;           ((:traversal-status @graph-state) edge-pair)]
      ;;          "\n")
      )))

(defn setup-graph-state-view!
  [target-el]
  (let [
        ]
    (rdom/render
     [(fn []
        (let [cur-graph-history-display-index (or (get-in @app-state [:graph-history-display :index])
                                                  (count @state-history))
              cur-restricted-max-index (get-in @app-state [:graph-history-display :max])
              cur-restricted-focused-step (if cur-restricted-max-index
                                            (-> (nth @state-history cur-restricted-max-index)
                                                :app-state
                                                :current-step))]
          [:div
           {:style {:width "100%"
                    :height "100%"}}
           [:div
            {:style {:width "100%"
                     :background "white"}}
            
            [:h4 (str "a panzoom test. "
                      (count @state-history)
                      " states in history (showing states "
                      (or (get-in @app-state [:graph-history-display :min]) 0)
                      " to "
                      (or (get-in @app-state [:graph-history-display :max])
                          (count @state-history))
                      ", focus on " cur-graph-history-display-index ")"
                      )]
            [:div
             {:style {:height "8em"}}
             (->> (concat [@app-state]
                          @state-history)
                  (map-indexed (fn [i app-state-step]
                                 ^{:key app-state-step}
                                 [:div
                                  {:style {:width "2em"
                                           :height "8em"
                                           :margin "1px"
                                           :float "left"
                                           :font-size "x-small"
                                           :font-family "monospace"}}
                                  [:div
                                   {:style {:transform "translateX(-40%) translateY(100%) rotate(-90deg)"
                                            :border "1px solid blue"
                                            :background (if (= i cur-graph-history-display-index)
                                                          "lime"
                                                          "")
                                            :width "8em"
                                            :height "2em"
                                            :overflow "hidden"
                                            :display "block"}}
                                   (get-in app-state-step [:app-state :current-step])]
                                  ])))
             ]
            [:div
             [:input
              {:type "range"
               :min (get-in @app-state [:graph-history-display :min])
               :max (if-let [num-history (count @state-history)]
                      num-history
                      0)
               :value (or (get-in @app-state [:graph-history-display :index])
                          (count @state-history))
               :style {:border "1px solid black"
                       :width "100%"}
               :on-change (fn [evt]
                            (let [val (get-evt-value evt)]
                              (swap! app-state assoc-in
                                     [:graph-history-display :index]
                                     (int val))))}]
             ]

            ]
           [:div
            {:class "enable-panzoom"
             :style {:width "800px"
                     :height "800px"
                     :border "2px solid purple"}}
            [:section
             { ;; :id "focal"
              :style {:border "1px solid orange"}}
             [:div
              { ;; :class "panzoom-parent"
               :style {:border"1px solid green"}}
              ]
             [:div
              [:input
               {:type "range"
                :class "zoom-range"}]
              [:button
               {:class "reset"}
               "reset"]
              ]]
            
            ;; draw state diagram
            [:div
             {:class "panzoom-block"}
             [:svg
              dagre/$svg-header
              dagre/$svg-defs
              ;; draw nodes
              (let [stroke "lightgrey"]
                (for [node (:node-seq @graph-state)]
                  [:g
                   (let [fill (if (nil? cur-restricted-max-index)
                                (if (= (:current-step @app-state)
                                       (:name node))
                                  "orange"
                                  "beige")
                                (if (= cur-restricted-focused-step
                                       (:name node))
                                  "salmon"
                                  "yellow"))
                         click-handler (fn [ev]
                                         (js/console.log "CLICKED ME!!!"))]
                     (case (:type node)
                       "choice" [:polygon {:points
                                           (let [x (- (:x node) (/ (:width node) 2))
                                                 y (- (:y node) (/ (:height node) 2))
                                                 w (:width node)
                                                 h (* 1.2 (:height node))
                                                 ]
                                             (apply str
                                                    (flatten
                                                     (interpose " "
                                                                (->>
                                                                 (partition 2
                                                                            [   x          (+ y (/ h 2)) 
                                                                             (+ x (/ w 2)) y
                                                                             (+ x w) (+ y (/ h 2))
                                                                             (+ x (/ w 2)) (+ y h)])
                                                                 (map #(interpose "," %))))))



                                             )
                                           :style {:fill fill
                                                   :stroke stroke}
                                           :onClick click-handler
                                           }]
                       "step" [:rect
                               (assoc {:x (- (:x node) (/ (:width node) 2))
                                       :y (- (:y node) (/ (:height node) 2))
                                       :width (:width node)
                                       :height (:height node)
                                       }
                                      :fill fill
                                      :stroke stroke
                                      :onClick click-handler)]))
                   [:text
                    (assoc {:x (- (:x node) (/ (:width node) 3))
                            :y (+ (- (:y node) (/ (:height node) 2))
                                  20)}
                           :fill "blue")
                    (:label node)]]))

              ;; draw edges
              (for [edge (:edge-seq @graph-state)]
                (let [midpt (grf/get-edge-midpt (:points edge))
                      edge-state (:state
                                  ((:traversal-status @graph-state)
                                   (vec (map edge [:v :w]))))]
                  [:g
                   [:text midpt
                    (:label edge)]
                   [:path
                    {:style (merge
                             (if (nil? edge-state)
                               {:stroke "#FEC"
                                :strokeWidth "2px"}
                               {:stroke "red"
                                :strokeWidth "4px"})
                             {:fill "none"
                              :markerEnd "url(#markerArrow)"})
                     :d (apply
                         str
                         "M"      
                         (interpose
                          " L"
                          (for [pt (:points edge)]
                            (str (:x pt) "," (:y pt)))))}]

                   ])
                )
              
              ]]]]))]
     target-el)

    (let [panzoom-container (js/$ ".enable-panzoom")
          panzoom-el
          (-> panzoom-container
              (.find ".panzoom-block")
              (.panzoom
               (clj->js
                {:$zoomRange (-> panzoom-container
                                 (.find ".zoom-range"))
                 :$reset (-> panzoom-container
                             (.find ".reset"))
                 :minScale 0.1
                 ;; will not work with SVG >:-(
                 ;; :contain "automatic"
                 })))

          ]

      (-> panzoom-el
          (.parent)
          (.on "mousewheel.focal"
               (fn [e]
                 (e.preventDefault)
                 (let [delta (or (aget e "delta")
                                 (aget e "originalEvent" "wheelDelta"))
                       zoom-out (if delta
                                  (< delta 0)
                                  (> (aget e "originalEvent" "deltaY") 0))
                       ]
                   (-> (aget panzoom-el "panzoom")
                       (.call panzoom-el
                              "zoom" zoom-out
                              (clj->js
                               {:increment 0.05
                                :animate false
                                :focal e})))
                   
                   )
                 )))
      )
    )
  )

(defn setup-graph-control-view! [target-el]
  (rdom/render
   [(fn []
      [:div
       [:h1 "controller"]
       [:button
        {:style {:width "100px"
                 :height "40px"
                 :fontSize "16pt"}
         :onClick (fn [_]
                    (swap! app-state assoc
                           :current-step (first (first my-transition-spec))))}
        "reset"]
       [:button
        {:style {:width "100px"
                 :height "40px"
                 :fontSize "16pt"}
         :onClick (fn [_]
                    (undo!))}
        "prev"]
       [:button
        {:style {:width "100px"
                 :height "40px"
                 :fontSize "16pt"}
         :onClick go-next-step!}
        "next"]
       ])]
   target-el))

(comment
  (setup-graph-state-view! (gdom/getElement "panel-A"))
  (setup-graph-control-view! (gdom/getElement "panel-B")))
