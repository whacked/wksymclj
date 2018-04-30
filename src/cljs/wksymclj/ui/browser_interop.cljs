(ns wksymclj.ui.browser-interop
  )


;; <dom / browser ui helper>
(defn get-evt-value [evt]
  (aget evt "target" "value"))
;; </dom / browser ui helper>

