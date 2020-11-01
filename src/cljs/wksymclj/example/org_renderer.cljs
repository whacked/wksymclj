(ns wksymclj.example.org-renderer
  (:require [goog.dom :as gdom]
            [reagent.core :as r]
            [cljs.nodejs :as nodejs]
            [cljs.pprint]
            
            [wksymclj.codec.org-mode :as wk-org]))

(def fs (nodejs/require "fs"))

(def org-src (fs.readFileSync
              "/home/vagrant/Desktop/wksymclj/moozorgjs.org"
              "utf-8"))

(cljs.pprint/pprint
 (wk-org/org2hiccup
        org-src))



(let [target-el (gdom/getElement "panel-B")]
  (js/console.clear)
  (r/render
   [:ul
    {:class "unordered-list"}
    ;; [:li {} [[[[:span ""]] [:span "options:"] [[:span ""]]]]]
    ;; [:li {} [[[[:span ""]] [:span "title:"] [[:span ""]]]]]
    ;; [:li {} [[[[:span ""]] [:span "author:"] [[:span ""]]]]]
    ;; [:li {} [[[[:span ""]] [:span "email:"] [[:span ""]]]]]
    [:li {}
     [[:span "FOO"] [:span "email:"] [[:span ""]]]]
    ]
   target-el))

(do
  (js/console.clear)
 (wk-org/org2hiccup
  "* hello\n\na *os* idfjioasdf"))

(comment
  (let [target-el (gdom/getElement "panel-B")]
    (js/console.clear)
    (r/render
     (->
      (concat
       [:div
        {:style {:height "100%"
                 :overflow "scroll"}}
        ]
       (wk-org/org2hiccup
        org-src))
      (vec)
      )
     target-el))
  
  (let [target-el (gdom/getElement "panel-A")
        org-src "* hello there\nmy text"]
    (r/render
     [(fn []
        [:div
         {:dangerouslySetInnerHTML
          {:__html (wk-org/org2html org-src)}}])]
     target-el)))
