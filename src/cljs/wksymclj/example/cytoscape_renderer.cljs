(ns wksymclj.example.cytoscape-renderer
  (:require [cljs.nodejs :as nodejs]
            [goog.dom :as gdom]))

;; this is a direct port of http://js.cytoscape.org/demos/visual-style/

;; need to add package
;; cytoscape

(def cytoscape (nodejs/require "cytoscape"))

(comment
  (cytoscape
   (clj->js
    {:container (gdom/getElement "panel-C")
     :layout {:name "cose"
              :padding 10}
     :style (-> (.stylesheet cytoscape)
                (.selector "node")
                (.css (clj->js
                       {:shape "data(faveShape)"
                        :width "mapData(weight, 40, 80, 20, 60)"
                        :content "data(name)"
                        :text-valign "center"
                        :text-outline-width 2
                        :text-outline-color "data(faveColor)"
                        :background-color "data(faveColor)"
                        :color "#fff"
                        }))
                (.selector ":selected")
                (.css (clj->js
                       {"border-width" 3
                        "border-color" "#333"}))
                (.selector "edge")
                (.css (clj->js
                       {"curve-style" "bezier"
                        "opacity" 0.666
                        "width" "mapData(strength, 70, 100, 2, 6)"
                        "target-arrow-shape" "triangle"
                        "source-arrow-shape" "circle"
                        "line-color" "data(faveColor)"
                        "source-arrow-color" "data(faveColor)"
                        "target-arrow-color" "data(faveColor)"
                        }))
                (.selector "edge.questionable")
                (.css (clj->js
                       {"line-style" "dotted"
                        "target-arrow-shape" "diamond"}))
                (.selector ".faded")
                (.css (clj->js
                       {"opacity" 0.25
                        "text-opacity" 0})))
     :elements (clj->js
                {:nodes 
                 [{ :data { :id "j" :name "Jerry" :weight 65 :faveColor "#6FB1FC" :faveShape "triangle" } }
                  { :data { :id "e" :name "Elaine" :weight 45 :faveColor "#EDA1ED" :faveShape "ellipse" } }
                  { :data { :id "k" :name "Kramer" :weight 75 :faveColor "#86B342" :faveShape "octagon" } }
                  { :data { :id "g" :name "George" :weight 70 :faveColor "#F5A45D" :faveShape "rectangle" } }]
                 :edges
                 [{ :data { :source "j" :target "e" :faveColor "#6FB1FC" :strength 90 } }
                  { :data { :source "j" :target "k" :faveColor "#6FB1FC" :strength 70 } }
                  { :data { :source "j" :target "g" :faveColor "#6FB1FC" :strength 80 } }
                  { :data { :source "e" :target "j" :faveColor "#EDA1ED" :strength 95 } }]
                 })

     :ready (fn []
              (this-as this
                (aset js/window "cy" this)))})))
