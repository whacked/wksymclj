(ns wksymclj.ui.colorization
  (:require [cljs.nodejs :as nodejs]))

(def ObjectHash (nodejs/require "object-hash"))
(def ColorHash (nodejs/require "color-hash"))

(defn to-hex-color [js-obj]
  (let [hash (ObjectHash js-obj)]
    (-> (new ColorHash)
        (.hex hash))))
