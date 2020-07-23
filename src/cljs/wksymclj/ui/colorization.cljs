;; TODO: add npm deps
;; npm install object-hash color-hash

(ns wksymclj.ui.colorization
  (:require ["object-hash" :as ObjectHash]
            ["color-hash" :as ColorHash]))

(defn to-hex-color [obj]
  (let [hash (ObjectHash (clj->js obj))]
    (-> (new ColorHash)
        (.hex hash))))
