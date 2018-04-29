(ns wksymclj.ui.golden-layout
  (:require-macros [swiss.arrows :refer [-<>]])
  (:require [reagent.core :as r]
            [goog.dom :as gdom]))

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

(defn setup-react-layout! [container & {:keys [on-complete
                                               layout]
                                        :or {on-complete init-window-event-handers!}}]
  (let [_component-name "my-component"
        layout-spec (or layout
                        (let [make-subcomponent (fn [label]
                                                  {:type "react-component"
                                                   :component _component-name
                                                   :props {:id (str "panel-" label)
                                                           :label label}})]
                          {:content
                           [{:type "row"
                             :content
                             [(make-subcomponent "A")
                              {:type "column"
                               :content
                               [(make-subcomponent "B")
                                (make-subcomponent "C")]}]}]}))
        
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
                            :height "100%"}})
            [:h1 "HELLO! "
             (:label props)
             ": " (:id props)]])}))
      (.init))))
