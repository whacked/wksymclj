(ns wksymclj.ui.panzoom)

(defn activate-panzoom! [element]
  (let [panzoom-container (js/$ element)
        panzoom-el
        (-> panzoom-container
            (.children)
            (.first)
            (.panzoom
             (clj->js
              {:minScale 0.1
               ;; will not work with SVG >:-(
               ;; :contain "automatic"
               })))]

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
                              :focal e})))))))))
