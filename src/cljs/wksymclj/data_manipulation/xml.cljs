;; TODO: add dep
;; npm install x2js

(ns wksymclj.data-manipulation.xml
  (:require ["x2js" :as X2JS]))

(defn xml->js [xml]
  (-> (new X2JS)
      (js-invoke "xml2js" xml)))

(defn js->xml [js-obj]
  (let [func (aget (new X2JS) "js2xml")]
    (func js-obj)))
