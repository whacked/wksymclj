(ns wksymclj.example.cytoscape-renderer
  (:require ["cytoscape" :as cytoscape]
            [goog.dom :as gdom]))

;; this is a direct port of http://js.cytoscape.org/demos/visual-style/

;; need to add package
;; npm install cytoscape

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


(comment
  ;; taken from http://js.cytoscape.org/#notation/elements-json
  (cytoscape
   (clj->js
    {:container (gdom/getElement "panel-C")
     :elements [{ ;; node n1
                 :group "nodes" ;; "nodes" for a node "edges" for an edge
                 ;; NB the group field can be automatically inferred for you but specifying it
                 ;; gives you nice debug messages if you mis-init elements


                 :data { ;; element data (put json serialisable dev data here)
                        :id "n1" ;; mandatory (string or number) id for each element assigned automatically on undefined
                        :parent "nparent" ;; indicates the compound node parent id; not defined => no parent
                        }

                 ;; scratchpad data (usually temp or nonserialisable data)
                 :scratch {
                           :_foo "bar" ;; app fields prefixed by underscore; extension fields unprefixed
                           }

                 :position { ;; the model position of the node (optional on init mandatory after)
                            :x 100
                            :y 100
                            }

                 :selected false ;; whether the element is selected (default false)

                 :selectable true ;; whether the selection state is mutable (default true)

                 :locked false ;; when locked a node"s position is immutable (default false)

                 :grabbable true ;; whether the node can be grabbed and moved by the user

                 :classes "foo bar" ;; a space separated list of class names that the element has
                 }

                { ;; node n2
                 :data { :id "n2" }
                 :renderedPosition { :x 200 :y 200 } ;; can alternatively specify position in rendered on-screen pixels
                 }

                { ;; node n3
                 :data { :id "n3" :parent "nparent" }
                 :position { :x 123 :y 234 }
                 }

                { ;; node nparent
                 :data { :id "nparent" :position { :x 200 :y 100 } }
                 }

                { ;; edge e1
                 :data {
                        :id "e1"
                        ;; inferred as an edge because `source` and `target` are specified:
                        :source "n1" ;; the source node id (edge comes from this node)
                        :target "n2" ;; the target node id (edge goes to this node)
                        }}
                ]

     :layout {:name "preset"}

     ;; so we can see the ids
     :style [{:selector "node"
              :style {:content "data(id)"}}]
     })))
