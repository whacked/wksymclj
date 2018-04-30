(ns wksymclj.data-manipulation.xml
  (:require [cljs.nodejs :as nodejs]))

(def X2JS (nodejs/require "x2js"))

(defn xml->js [xml]
  (-> (new X2JS)
      (js-invoke "xml2js" xml)))

(defn js->xml [js-obj]
  (let [func (aget (new X2JS) "js2xml")]
    (func js-obj)))
