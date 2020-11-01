(ns ui.parking
  (:require [cljs-css-modules.macro :refer-macros [defstyle]]))

(let [bgcolor ""]
  (defstyle my-style
    ["#glpanel-1" {:background bgcolor
                   :width "100%"
                   :height "100%"
                   :overflow "scroll"}]))



(comment
  ;; panzoom tiger test
  ;; this should be executed in the ui.graph namespace


  (comment
    ;; just load the panzoom demo in an iframe.
    ;; the panzoom functionality works 100%

    (r/render
     [:div
      {:style {:width "100%"
               :height "800px"
               :border "2px solid red"}}
      [:iframe
       {:src "http://timmywil.github.io/jquery.panzoom/demo/"
        :style {:width "100%"
                :height "100%"}}]]
     (js/document.getElementById "app")))

  
  ;; here, the panzoom doesn't work 100%
  ;; mousewheel zoom works, but any pan attempt abruptly resets the view,
  ;; if you are running an electron instance via VNC. direct electron works
  (doto (js/document.getElementById "app")
    (aset "innerHTML" ""))

  (r/render
   [:div
    {:style {:width "800px"
             :height "800px"
             :border "2px solid red"}}
    [:section#focal
     [:h1 "Use the mousewheel to zoom on a focal point"]
     [:div.parent
      [:div.panzoom
       [:img
        {:height "500"
         :width "500"
         :src "http://blog.millermedeiros.com/wp-content/uploads/2010/04/awesome_tiger.svg"}]]]]]
   (js/document.getElementById "app"))

  (let [section (js/$ "#focal")
        panzoom (-> section
                    (.find ".panzoom")
                    (.panzoom))]
    (-> panzoom
        (.parent)
        (.on "mousewheel.focal"
             (fn [e]
               (.preventDefault e)
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
                 
                 ))))
    )


  )
