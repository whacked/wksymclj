(ns wksymclj.ui.browser-interop
  )

(defn get-evt-value [evt]
  (aget evt "target" "value"))

(defn clear-dom-element! [el]
  (doto el
    (aset "innerHTML" "")))

(defn set-element-style! [dom-element style-map]
  (when dom-element
    (doto dom-element
      (-> (aget "style")
          (js/Object.assign
           (clj->js style-map))))))
