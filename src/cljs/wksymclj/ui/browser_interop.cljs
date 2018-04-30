(ns wksymclj.ui.browser-interop
  )

(defn get-evt-value [evt]
  (aget evt "target" "value"))

(defn clear-dom-element! [el]
  (doto el
    (aset "innerHTML" "")))
