(ns wksymclj.ui.golden-layout
  (:require-macros [swiss.arrows :refer [-<>]])
  (:require [reagent.core :as r]
            ;; GL needs these in window namespace
            [react]
            [react-dom]
            ["jquery" :as jquery]
            ["golden-layout" :as golden-layout]
            [goog.dom :as gdom]))

;; GoldenLayout compat
(aset js/window "React" react)
(aset js/window "ReactDOM" react-dom)
(aset js/window "$" jquery)
(aset js/window "GoldenLayout" golden-layout)


(comment
  ;; this follows the basic example (NOT the ReactJS version)
  ;; from http://golden-layout.com/tutorials/getting-started.html

  (defn golden-layout-demo! []
    (let [main-container (gdom/getElement "app")
          _component-name "test-component"
          make-subcomponent (fn [label]
                              {:type "component"
                               :componentName _component-name
                               :componentState { :label label }})
          config {:content
                  [{:type "row"
                    :content
                    [(make-subcomponent "A")
                     {:type "column"
                      :content
                      [(make-subcomponent "B")
                       (make-subcomponent "C")]}]}]}
          gl-layout (js/GoldenLayout.
                     (clj->js config)
                     main-container)]

      ;; empty the container?
      (doto main-container
        (aset "innerHTML" "")
        (aset "style" "width:100%;height:800px;"))

      (doto 
          gl-layout
        (.registerComponent
         _component-name
         (fn [container component-state]
           (-> container
               (.getElement)
               (.html (str "<h2>STUFF: "
                           (.-label component-state)
                           "</h2>")))))
        (.init)))))

(def event-handlers
  (atom {:resize []}))

(defn init-window-event-handers! []
  (doto js/window
    (.addEventListener
     "resize"
     (fn [evt]
       (doseq [event-handler (:resize @event-handlers)]
         (event-handler evt))))))

(defn make-react-subcomponent [component-name label]
  (let [panel-id (str "panel-" label)]
    {:type "react-component"
     :component component-name
     :title panel-id
     :props {:id panel-id
             :label label}}))

(defn convert-panel-config-edn [component-name panel-config]
  (if-let [react-component-label
           (:react-component panel-config)]
    (make-react-subcomponent
     component-name
     react-component-label)
    (update panel-config
            :content (fn [sub-panel-coll]
                       (map (partial
                             convert-panel-config-edn
                             component-name)
                            sub-panel-coll)))))

(defn setup-react-layout! [container & {:keys [on-complete
                                               layout]
                                        :or {on-complete init-window-event-handers!}}]
  (let [_component-name "my-component"
        _panel-container-class "panel-container"
        layout-spec (convert-panel-config-edn
                     _component-name
                     (or layout
                         {:content
                          [{:type "row"
                            :content
                            [{:react-component "A"}
                             {:type "column"
                              :content
                              [{:react-component "B"}
                               {:react-component "C"}]}
                             {:react-component "D"}]}]}))
        
        gl-layout (js/GoldenLayout.
                   (clj->js layout-spec)
                   container)]
    
    ;; empty the container?
    (doto container
      (aset "innerHTML" "")
      (aset "style" "width:100%;height:100%;"))

    (swap! event-handlers
           update-in [:resize]
           conj (fn [e]
                  (.updateSize gl-layout)))

    (when on-complete
      (.on gl-layout "initialised"
           on-complete))
    (doto
        gl-layout
      (.registerComponent
       _component-name
       (r/create-class
        {:reagent-render
         (fn [props]
           [:div
            (merge (select-keys props [:id])
                   {:style {:display "flex"
                            :width "100%"
                            :height "100%"}}
                   {:dangerouslySetInnerHTML
                    {:__html
                     (str "<div class=\"" _panel-container-class "\">"
                          "<h2>"
                          (:label props) " @ " (:id props)
                          "</h2>"
                          "</div>")}})])}))
      (.init))))
