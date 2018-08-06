(ns wksymclj.ui.colorization
  (:require [cljs.nodejs :as nodejs]))

(def ObjectHash (nodejs/require "object-hash"))
(def ColorHash (nodejs/require "color-hash"))

(defn to-hex-color [obj]
  (let [hash (ObjectHash (clj->js obj))]
    (-> (new ColorHash)
        (.hex hash))))
